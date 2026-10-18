;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'cl-lib)
(require 'seq)
(require 'thingatpt)
(require 'treesit nil t)

(defvar mc/cmds-to-run-for-all)
(defvar mc/cursor-specific-vars)
(declare-function mc/all-fake-cursors "multiple-cursors-core" ())

(defvar-local adh--match-last-selection nil
  "Last match selection: object, around flag, bounds and buffer edit tick.
Used to grow repeated scope selections; each multiple cursor keeps its own.")

(defconst adh--match-pairs
  '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}) (?< . ?>)
    (?\" . ?\") (?' . ?') (?` . ?`))
  "Delimiter pairs accepted by the match commands.")

(defun adh--match-pair (char)
  "Return the delimiter pair identified by either end CHAR."
  (or (assq char adh--match-pairs) (rassq char adh--match-pairs)
      (cons char char)))

(defun adh--match-position ()
  "Return the character under the selection's cursor.
Emacs puts point after the last selected character in a forward region."
  (if (and (use-region-p) (> (point) (mark))) (1- (point)) (point)))

(defun adh--match-select (bounds &optional backward)
  "Activate BOUNDS, with point at the beginning if BACKWARD is non-nil."
  (unless (and bounds (<= (point-min) (car bounds) (cdr bounds) (point-max)))
    (user-error "No matching text object here"))
  (push-mark (if backward (cdr bounds) (car bounds)) t t)
  (goto-char (if backward (car bounds) (cdr bounds)))
  (setq deactivate-mark nil))

(defun adh--match-read-char (prompt)
  "Read an object or delimiter with PROMPT, allowing Escape to cancel."
  (let ((char (read-char prompt)))
    (when (eq char ?\e) (keyboard-quit))
    char))

(defun adh--match-escaped-p (pos)
  "Return non-nil if the character at POS follows an odd number of escapes."
  (save-excursion
    (goto-char pos)
    (let ((end (point)))
      (skip-chars-backward "\\\\")
      (cl-oddp (- end (point))))))

(defun adh--match-pair-candidates (pair pos)
  "Return enclosing PAIR bounds at POS, respecting strings and comments."
  (save-excursion
    (let* ((open (car pair)) (close (cdr pair))
           (symmetric (= open close))
           (state (syntax-ppss pos))
           (context (nth 8 state))
           (regexp (regexp-opt (list (char-to-string open)
                                     (char-to-string close))))
           stacks bounds)
      (when symmetric
        (let* ((string-state (if (nth 3 state) state
                               (syntax-ppss (min (point-max) (1+ pos)))))
               (start (and (nth 3 string-state) (nth 8 string-state)))
               (end (and start (eq (char-after start) open)
                         (ignore-errors (scan-sexps start 1)))))
          (when (and end (<= start pos) (< pos end))
            (push (cons start end) bounds))))
      (goto-char pos)
      (let ((limit (if symmetric (line-end-position) (point-max))))
        (goto-char (if symmetric (line-beginning-position) (point-min)))
        (while (re-search-forward regexp limit t)
          (let* ((at (match-beginning 0))
                 (token-context (and (not symmetric)
                                     (nth 8 (save-excursion (syntax-ppss at))))))
            (unless (or (adh--match-escaped-p at)
                        (and token-context (not (equal context token-context))))
              (let ((stack (alist-get token-context stacks)))
                (if (and (= (char-after at) open)
                         (or (not symmetric) (null stack)))
                    (push at stack)
                  (when-let* ((start (pop stack)))
                    (when (and (<= start pos) (<= pos at))
                      (push (cons start (1+ at)) bounds))))
                (setf (alist-get token-context stacks) stack))))))
      (delete-dups bounds))))

(defun adh--match-pair-bounds (char count pos &optional enclose-region)
  "Find the COUNTth enclosing CHAR pair at POS; m means the nearest pair.
If ENCLOSE-REGION is non-nil, require the pair to enclose the active region."
  (let* ((pairs (if (eq char ?m) adh--match-pairs
                  (list (adh--match-pair char))))
         (bounds (cl-mapcan (lambda (pair) (adh--match-pair-candidates pair pos)) pairs)))
    (when (and enclose-region (use-region-p))
      (setq bounds (seq-filter (lambda (b) (and (<= (car b) (region-beginning))
                                                (>= (cdr b) (region-end))))
                               bounds)))
    (nth (1- count) (sort bounds (lambda (a b) (< (- (cdr a) (car a))
                                                  (- (cdr b) (car b))))))))

(defun adh--match-word-bounds (long around pos)
  "Return word bounds at POS; LONG selects a WORD, AROUND includes spacing."
  (save-excursion
    (goto-char pos)
    (unless (or (eobp) (looking-at-p "[[:space:]]"))
      (let ((chars (cond (long "^ \t\r\n")
                         ((looking-at-p "[[:alnum:]_]") "[:alnum:]_")
                         (t "^[:alnum:]_ \t\r\n")))
            beg end)
        (skip-chars-backward chars)
        (setq beg (point))
        (skip-chars-forward chars)
        (setq end (point))
        (when around
          (skip-chars-forward " \t")
          (if (> (point) end) (setq end (point))
            (goto-char beg)
            (skip-chars-backward " \t")
            (setq beg (point))))
        (cons beg end)))))

(defun adh--match-paragraph-bounds (around count pos)
  "Return COUNT paragraphs at POS, including trailing blank lines if AROUND."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (while (and (not (eobp)) (looking-at-p "[ \t]*$")) (forward-line 1))
    (when (eobp)
      (skip-chars-backward " \t\r\n")
      (beginning-of-line))
    (unless (looking-at-p "[ \t]*$")
      (while (and (> (point) (point-min))
                  (save-excursion (forward-line -1) (not (looking-at-p "[ \t]*$"))))
        (forward-line -1))
      (let ((beg (point)) end)
        (dotimes (_ count)
          (while (and (not (eobp)) (not (looking-at-p "[ \t]*$"))) (forward-line 1))
          (setq end (point))
          (while (and (not (eobp)) (looking-at-p "[ \t]*$")) (forward-line 1)))
        (cons beg (if around (point) end))))))

(defun adh--match-defun-bounds (around pos)
  "Return a defun at POS without requiring a Tree-sitter parser.
For Lisp, inside means the forms after the name and argument list."
  (save-excursion
    (goto-char pos)
    (when-let* ((bounds (bounds-of-thing-at-point 'defun)))
      (goto-char (car bounds))
      (skip-chars-forward " \t\r\n")
      (let ((beg (point))
            (end (progn (goto-char (cdr bounds))
                        (skip-chars-backward " \t\r\n") (point))))
        (when (and (<= beg pos) (< pos end))
          (cond
           (around (cons beg end))
           ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'lisp-data-mode 'scheme-mode)
            (goto-char beg)
            (when (looking-at "(\\(?:cl-\\)?\\(?:defun\\|defmacro\\|define\\)\\_>")
              (forward-char 1)
              (forward-sexp (if (looking-at-p "define\\_>") 2 3))
              (skip-chars-forward " \t\r\n")
              (cons (min (point) (1- end)) (1- end))))
           ((eq (char-before end) ?\})
            (goto-char end)
            (backward-sexp)
            (when (eq (char-after) ?\{) (cons (point) end)))))))))

(defun adh--match-comment-bounds (around pos)
  "Return the comment at POS, including adjacent comments when AROUND."
  (save-excursion
    (goto-char pos)
    (let* ((state (syntax-ppss pos))
           (state (if (nth 4 state) state
                    (syntax-ppss (min (point-max) (+ pos 2)))))
           (beg (and (nth 4 state) (nth 8 state))))
      (when (and beg (<= beg pos))
        (goto-char beg)
        (forward-comment 1)
        (let ((end (point)))
          (when around
            (while (and (forward-comment 1) (> (point) end)) (setq end (point)))
            (goto-char beg)
            (while (and (forward-comment -1) (< (point) beg)) (setq beg (point))))
          (cons beg end))))))

(defconst adh--match-tree-types
  '((?f "function_definition" "function_item" "function_declaration"
        "method_definition" "method_declaration" "lambda_expression"
        "closure_expression" "arrow_function" "function_expression"
        "generator_function_declaration" "func_literal")
    (?t "class_definition" "class_specifier" "class_declaration"
        "struct_specifier" "struct_item" "enum_specifier" "enum_item"
        "union_specifier" "union_item" "trait_item" "impl_item"
        "interface_declaration" "class" "type_declaration")
    (?c "comment" "line_comment" "block_comment")
    (?x "element" "script_element" "style_element"))
  "Shared grammar node names for syntax-aware text objects.
Unsupported grammars simply have no matching object.")

(defconst adh--match-tree-list-types
  '((?a "parameter_list" "parameters" "formal_parameters" "lambda_parameters"
        "closure_parameters" "argument_list" "arguments"
        "type_parameters" "type_parameter_list" "type_arguments")
    (?e "initializer_list" "field_initializer_list" "array_expression"
        "tuple_expression" "tuple_pattern" "list" "tuple" "set"
        "dictionary" "object" "array"))
  "Container nodes whose named children are arguments or entries.")

(defun adh--match-tree-span (node)
  "Return the buffer bounds of NODE."
  (cons (treesit-node-start node) (treesit-node-end node)))

(defun adh--match-tree-test-p (node)
  "Whether NODE is a Python/Go test function or Rust attributed test."
  (when (member (treesit-node-type node)
                (cdr (assq ?f adh--match-tree-types)))
    (let ((name (treesit-node-child-by-field-name node "name"))
          (previous (treesit-node-prev-sibling node t))
          found)
      (or (and name (string-match-p
                     (if (eq (treesit-node-language node) 'go)
                         "\\`\\(?:Test\\|Benchmark\\|Example\\|Fuzz\\)" "\\`test_")
                     (treesit-node-text name t)))
          (progn
            (while (and previous
                        (member (treesit-node-type previous)
                                '("attribute_item" "line_comment")))
              (when (and (equal (treesit-node-type previous) "attribute_item")
                         (string-match-p "#\\[\\(?:[[:alnum:]_]+::\\)?test\\]"
                                         (treesit-node-text previous t)))
                (setq found t))
              (setq previous (treesit-node-prev-sibling previous t)))
            found)))))

(defun adh--match-tree-list-bounds (node object around count pos)
  "Select COUNT children of container NODE at POS for OBJECT.
AROUND includes a following comma, like Helix's parameter object."
  (let ((index 0) children selected)
    (while (< index (treesit-node-child-count node t))
      (let ((child (treesit-node-child node index t)))
        (unless (member (treesit-node-type child)
                        (cdr (assq ?c adh--match-tree-types)))
          (push child children)))
      (setq index (1+ index)))
    (setq children (nreverse children))
    (while (and children (<= (treesit-node-start (car children)) pos))
      (setq selected children children (cdr children)))
    (setq selected (or selected children))
    (when selected
      (let* ((first (car selected))
             (last first)
             (remaining (1- count)))
        (while (and (> remaining 0) (cdr selected))
          (setq selected (cdr selected) last (car selected)
                remaining (1- remaining)))
        (when (zerop remaining)
          (let ((beg (treesit-node-start first))
                (end (treesit-node-end last)))
            (when (and (eq object ?e) (not around)
                       (equal (treesit-node-type first) "pair")
                       (treesit-node-eq first last))
              (let ((part (or (treesit-node-first-child-for-pos first pos t)
                              (treesit-node-child first -1 t))))
                (when part (setq beg (treesit-node-start part)
                                 end (treesit-node-end part)))))
            (when around
              (save-excursion
                (goto-char end)
                (skip-chars-forward " \t\n\r" (treesit-node-end node))
                (when (eq (char-after) ?,) (setq end (1+ (point))))))
            (cons beg end)))))))

(defun adh--match-tree-object-bounds (node object around pos)
  "Return NODE's bounds for OBJECT, including its wrapper if AROUND."
  (cond
   ((and (eq object ?c) around)
    (let ((beg (treesit-node-start node)) (end (treesit-node-end node))
          (previous node) (next node))
      (while (and (setq previous (treesit-node-prev-sibling previous t))
                  (member (treesit-node-type previous)
                          (cdr (assq ?c adh--match-tree-types))))
        (setq beg (treesit-node-start previous)))
      (while (and (setq next (treesit-node-next-sibling next t))
                  (member (treesit-node-type next)
                          (cdr (assq ?c adh--match-tree-types))))
        (setq end (treesit-node-end next)))
      (cons beg end)))
   ((and (eq object ?x) (not around))
    (let ((opening (treesit-node-child node 0))
          (closing (treesit-node-child node -1)))
      (when (and opening closing
                 (member (treesit-node-type opening) '("start_tag" "script_start_tag"))
                 (equal (treesit-node-type closing) "end_tag"))
        (cons (treesit-node-end opening) (treesit-node-start closing)))))
   ((and (eq object ?t) (not around)
         (equal (treesit-node-type node) "type_declaration"))
    (let* ((spec (treesit-node-first-child-for-pos node pos t))
           (type (and spec (treesit-node-child-by-field-name spec "type"))))
      (when type
        (pcase (treesit-node-type type)
          ("struct_type"
           (when-let* ((fields (treesit-node-child type 0 t)))
             (adh--match-tree-span fields)))
          ("interface_type"
           (when-let* ((first (treesit-node-child type 0 t))
                       (last (treesit-node-child type -1 t)))
             (cons (treesit-node-start first) (treesit-node-end last))))))))
   ((and (memq object '(?f ?t ?T)) (not around))
    (when-let* ((body (treesit-node-child-by-field-name node "body")))
      (adh--match-tree-span body)))
   (t
    (let* ((parent (treesit-node-parent node))
           (beg (treesit-node-start node)) (end (treesit-node-end node))
           (previous (treesit-node-prev-sibling node t)))
      (when (and parent (equal (treesit-node-type parent) "decorated_definition"))
        (setq beg (treesit-node-start parent) end (treesit-node-end parent)))
      (when (memq object '(?f ?T))
        (while (and previous
                    (or (equal (treesit-node-type previous) "attribute_item")
                        (and (eq object ?T)
                             (equal (treesit-node-type previous) "line_comment"))))
          (setq beg (treesit-node-start previous)
                previous (treesit-node-prev-sibling previous t))))
      (cons beg end)))))

(defun adh--match-tree-parser-bounds (object around count pos parser)
  "Find OBJECT's bounds in PARSER at POS using AROUND and COUNT."
  (let ((node (treesit-node-at pos parser))
        (list-types (cdr (assq object adh--match-tree-list-types)))
        result)
    (while (and node (not result))
      (when (and (<= (treesit-node-start node) pos)
                 (< pos (treesit-node-end node)))
        (if list-types
            (when (member (treesit-node-type node) list-types)
              (setq result (adh--match-tree-list-bounds
                            node object around count pos))
              (setq node nil))
          (when (if (eq object ?T) (adh--match-tree-test-p node)
                  (member (treesit-node-type node)
                          (cdr (assq object adh--match-tree-types))))
            (setq count (1- count))
            (when (zerop count)
              (setq result (adh--match-tree-object-bounds node object around pos)
                    node nil)))))
      (when node (setq node (treesit-node-parent node))))
    result))

(defun adh--match-tree-bounds (object around count pos)
  "Return bounds for syntax OBJECT at POS, or nil if unavailable.
AROUND selects the complete object; otherwise select its body.
COUNT chooses an enclosing object, or consecutive arguments/entries.
Recognize common node names in existing parsers, trying embedded languages
before the host language.  No parser or grammar is created or downloaded."
  (when (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (let ((parsers (if (fboundp 'treesit-parsers-at)
                       (treesit-parsers-at pos) (treesit-parser-list)))
          result)
      (while (and parsers (not result))
        (setq result (adh--match-tree-parser-bounds
                      object around count pos (pop parsers))))
      result)))

(defun adh--match-object-bounds (object around count)
  "Return bounds for OBJECT, AROUND or inside, with prefix COUNT."
  (unless (> count 0) (user-error "The match count must be positive"))
  (let ((pos (adh--match-position)))
    ;; Keep an unchanged syntax selection anchored inside its node when
    ;; switching around/inside or repeating.  Its visible beginning may
    ;; be a decorator, attribute or opening tag outside that node.
    (when (and (memq object '(?f ?t ?T ?x)) (use-region-p)
               (eq object (car adh--match-last-selection))
               (equal (nthcdr 2 adh--match-last-selection)
                      (list (region-beginning) (region-end)
                            (buffer-chars-modified-tick))))
      (setq pos (1- (region-end))))
    (pcase object
      ((or ?w ?W) (adh--match-word-bounds (eq object ?W) around pos))
      (?p (adh--match-paragraph-bounds around count pos))
      ((or ?f ?t ?a ?c ?T ?e ?x)
       (or (adh--match-tree-bounds object around count pos)
           (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
             (pcase object
               (?f (adh--match-defun-bounds around pos))
               (?c (adh--match-comment-bounds around pos))))))
      (_ (when (and (not (eq object ?m))
                    (string-match-p "[[:alnum:][:space:]]" (char-to-string object)))
           (user-error "Unknown text object: %c" object))
         (when-let* ((b (adh--match-pair-bounds object count pos)))
           (if around b (cons (1+ (car b)) (1- (cdr b)))))))))

(defun adh--match-select-object (object around count)
  "Select OBJECT with AROUND and COUNT, growing a repeated scope selection."
  (let* ((key (car (adh--match-pair object)))
         (selection (and (region-active-p)
                         (list key around (region-beginning) (region-end)
                               (buffer-chars-modified-tick))))
         (bounds (adh--match-object-bounds object around count)))
    (when (and (= count 1) selection
               (equal selection adh--match-last-selection)
               (not (memq object '(?w ?W ?p ?a ?c ?e))))
      (let ((beg (region-beginning)) (end (region-end)) previous)
        (while (and bounds
                    (not (and (<= (car bounds) beg) (>= (cdr bounds) end)
                              (or (< (car bounds) beg) (> (cdr bounds) end)))))
          (setq previous bounds
                count (1+ count)
                bounds (adh--match-object-bounds object around count))
          (when (equal bounds previous) (setq bounds nil))))
      (unless bounds (user-error "No larger matching scope here")))
    (adh--match-select bounds t)
    (setq adh--match-last-selection
          (list key around (car bounds) (cdr bounds) (buffer-chars-modified-tick)))))

(defun adh-match-inside (object count)
  "Select inside OBJECT, using COUNT for enclosing pairs or paragraphs.
Objects: w/W word/WORD, p paragraph, m nearest pair, literal delimiters,
f function, t type, a argument, c comment, T test, e entry, x element.
Leave point at the beginning and mark at the end.
Repeat on an unchanged scope with COUNT 1 to select the enclosing scope."
  (interactive (list (adh--match-read-char "Inside: w W p m ( [ { < \" ' ` f t a c T e x: ")
                     (prefix-numeric-value current-prefix-arg)))
  (adh--match-select-object object nil count))

(defun adh-match-around (object count)
  "Select around OBJECT; see `adh-match-inside' for objects and COUNT."
  (interactive (list (adh--match-read-char "Around: w W p m ( [ { < \" ' ` f t a c T e x: ")
                     (prefix-numeric-value current-prefix-arg)))
  (adh--match-select-object object t count))

(defun adh-match-jump (count)
  "Jump to the matching delimiter, or the COUNTth enclosing closing one."
  (interactive "p")
  (unless (> count 0) (user-error "The match count must be positive"))
  (let* ((pos (adh--match-position))
         (char (char-after pos))
         (bounds (adh--match-pair-bounds
                  (if (or (assq char adh--match-pairs) (rassq char adh--match-pairs))
                      char ?m)
                  count pos)))
    (unless bounds (user-error "No matching delimiter here"))
    (goto-char (if (= pos (1- (cdr bounds))) (car bounds) (1- (cdr bounds))))
    (deactivate-mark)))

(defun adh-match-surround (char)
  "Surround the active region (or character at point) with CHAR's pair."
  (interactive (list (adh--match-read-char "Surround with: ")))
  (let* ((pair (adh--match-pair char))
         (backward (or (not (use-region-p)) (< (point) (mark))))
         (beg (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (min (point-max) (1+ (point))))))
    (save-excursion
      (atomic-change-group
        (goto-char end) (insert (cdr pair))
        (goto-char beg) (insert (car pair))))
    (adh--match-select (cons beg (+ end 2)) backward)))

(defun adh--match-check-surround-cursors (old count bounds)
  "Check that each cursor has a distinct OLD surround before editing.
BOUNDS is the real cursor's target; COUNT chooses the enclosing pair.
Check all targets before any edits, so replay cannot reach an outer pair
after an earlier cursor removes or replaces a shared inner pair."
  (when (and (bound-and-true-p multiple-cursors-mode)
             (not (bound-and-true-p mc--executing-command-for-fake-cursor)))
    (let ((seen (list bounds)))
      (dolist (cursor (mc/all-fake-cursors))
        (let ((target
               (save-mark-and-excursion
                 (let ((mark-active (overlay-get cursor 'mark-active))
                       (transient-mark-mode
                        (overlay-get cursor 'transient-mark-mode)))
                   (goto-char (overlay-get cursor 'point))
                   (set-marker (mark-marker) (overlay-get cursor 'mark))
                   (adh--match-pair-bounds
                    old count (adh--match-position) t)))))
          (unless target
            (user-error "No matching surround at every cursor"))
          (when (member target seen)
            (user-error "Multiple cursors share the same surround"))
          (push target seen))))))

(defun adh--match-edit-surround (old new count)
  "Replace OLD's COUNTth enclosing pair with NEW, or delete it if NEW is nil."
  (unless (> count 0) (user-error "The match count must be positive"))
  (let ((bounds (adh--match-pair-bounds old count (adh--match-position) t)))
    (unless bounds (user-error "No matching surround here"))
    (adh--match-check-surround-cursors old count bounds)
    (let ((beg (car bounds)) (end (1- (cdr bounds)))
          (pair (and new (adh--match-pair new)))
          (pt (point)) (mk (mark t)) (active mark-active))
      (save-excursion
        (atomic-change-group
          (goto-char end) (delete-char 1) (when pair (insert (cdr pair)))
          (goto-char beg) (delete-char 1) (when pair (insert (car pair)))))
      (cl-flet ((adjust (p) (if pair p (- p (if (> p end) 1 0) (if (> p beg) 1 0)))))
        (goto-char (adjust pt))
        (when mk (set-mark (adjust mk))))
      (setq mark-active active deactivate-mark nil))))

(defun adh-match-replace-surround (old new count)
  "Replace OLD's enclosing pair with NEW, using COUNT for nesting; m is nearest."
  (interactive (list (adh--match-read-char "Replace surround (m for nearest): ")
                     (adh--match-read-char "Replace with: ")
                     (prefix-numeric-value current-prefix-arg)))
  (adh--match-edit-surround old new count))

(defun adh-match-delete-surround (char count)
  "Delete CHAR's enclosing pair, using COUNT for nesting; m is nearest."
  (interactive (list (adh--match-read-char "Delete surround (m for nearest): ")
                     (prefix-numeric-value current-prefix-arg)))
  (adh--match-edit-surround char nil count))

(defvar-keymap adh-match-map
  :doc "Helix match: inside/around, matching delimiters and surrounds."
  "i" #'adh-match-inside
  "a" #'adh-match-around
  "m" #'adh-match-jump
  "s" #'adh-match-surround
  "r" #'adh-match-replace-surround
  "d" #'adh-match-delete-surround)

(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc/cursor-specific-vars 'adh--match-last-selection)
  (dolist (command '(adh-match-inside adh-match-around adh-match-jump
                                      adh-match-surround adh-match-replace-surround adh-match-delete-surround))
    (add-to-list 'mc/cmds-to-run-for-all command)))

(provide 'adh-match)

;;; adh-match.el ends here

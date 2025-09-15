;;; shellspec-mode.el --- Major mode for editing shellspec test files -*- lexical-binding: t -*-

;;; Commentary:
;; This mode provides proper indentation and syntax highlighting for shellspec test files.
;; Shellspec is a BDD testing framework for shell scripts.

;;; Code:

(require 'sh-script)

;; Custom indentation function for shellspec DSL
(defun shellspec-indent-line ()
  "Indent current line for shellspec mode."
  (interactive)
  (let ((current-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
        (shellspec-keyword nil)
        (current-point (point))
        (beginning-pos (line-beginning-position)))
    
    ;; If current line is empty (only whitespace), remove all indentation
    (if (string-match "^\\s-*$" current-line)
        (progn
          (delete-region (line-beginning-position) (line-end-position))
          (setq shellspec-keyword t))
      
      ;; Check if current line has shellspec End keyword
      (when (string-match "^\\s-*End\\>" current-line)
        ;; Find the matching opening keyword and align with it
        (save-excursion
          (let ((depth 1)
                (found-pos nil))
            (while (and (> depth 0) (not (bobp)))
              (forward-line -1)
              (cond
               ((looking-at "^\\s-*End\\>")
                (setq depth (1+ depth)))
               ((looking-at "^\\s-*\\(Describe\\|Context\\|It\\|Specify\\|Example\\)\\>")
                (setq depth (1- depth))
                (when (= depth 0)
                  (setq found-pos (current-indentation))))))
            (when found-pos
              (indent-line-to found-pos)
              (setq shellspec-keyword t)))))
      
      ;; Check if previous line has shellspec keywords that affect current line indentation
      (unless shellspec-keyword
        (save-excursion
          (let ((prev-indent 0))
            (forward-line -1)
            ;; Skip empty lines
            (while (and (not (bobp)) 
                       (string-match "^\\s-*$" (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position))))
              (forward-line -1))
            
            (setq prev-indent (current-indentation))
            
            (cond
             ;; After Describe, Context, It, etc. - current line should be indented by 4 more
             ((looking-at "^\\s-*\\(Describe\\|Context\\|It\\|Specify\\|Example\\|Before\\|After\\|BeforeAll\\|AfterAll\\|BeforeEach\\|AfterEach\\|BeforeCall\\|AfterCall\\)\\>")
              ;; Go back to current line and indent it
              (goto-char beginning-pos)
              (indent-line-to (+ prev-indent 4))
              (setq shellspec-keyword t))
             
             ;; After When or The - current line maintains same indentation
             ((looking-at "^\\s-*\\(When\\|The\\)\\>")
              (goto-char beginning-pos)
              (indent-line-to prev-indent)
              (setq shellspec-keyword t))))))
      
      ;; If not a shellspec keyword context, use standard bash indentation
      (unless shellspec-keyword
        (sh-indent-line))
      
      ;; Restore point position relative to line beginning
      (when (> (- current-point beginning-pos) (current-indentation))
        (goto-char (+ (line-beginning-position) (current-indentation)))))))

;;;###autoload
(define-derived-mode shellspec-mode sh-mode "Shellspec"
  "Major mode for editing shellspec test files."
  ;; Set indentation to 4 spaces (your preferred style)
  (setq-local sh-basic-offset 4)
  (setq-local sh-indentation 4)
  (setq-local indent-tabs-mode nil)  ; Use spaces, not tabs
  
  ;; Disable aggressive shell indentation that fights with shellspec structure
  (setq-local sh-indent-for-case-label 0)
  (setq-local sh-indent-for-case-alt '+)
  (setq-local sh-indent-after-function '+)
  (setq-local sh-indent-after-open '+)
  (setq-local sh-indent-after-loop-construct '+)
  
  ;; Set our custom indentation function
  (setq-local indent-line-function 'shellspec-indent-line)
  
  ;; Define font-lock keywords for shellspec DSL
  ;; We inherit from sh-mode, so just add our keywords
  (font-lock-add-keywords
   nil
   '(("\\<Describe\\|Context\\|It\\|Specify\\|Example\\|When\\|The\\|End\\|Skip\\|Pending\\|Include\\|Before\\|After\\|BeforeAll\\|AfterAll\\|BeforeEach\\|AfterEach\\|BeforeCall\\|AfterCall\\|Parameters\\|Data\\>" 
      . font-lock-keyword-face)
     ("\\<should\\|should_not\\>" 
      . font-lock-builtin-face)
     ("\\<eq\\|ne\\|equal\\|not_equal\\|be\\|succeed\\|fail\\|include\\|start_with\\|end_with\\|match\\|satisfy\\|be_defined\\|be_undefined\\|be_present\\|be_blank\\|be_true\\|be_false\\|be_truthy\\|be_falsy\\|be_empty\\|be_exist\\|be_file\\|be_directory\\|be_symlink\\|be_pipe\\|be_socket\\|be_readable\\|be_writable\\|be_executable\\|be_block_device\\|be_character_device\\|have\\|be_valid_number\\|be_valid_funcname\\>"
      . font-lock-function-name-face))
   t))

;;;###autoload
(add-to-list 'auto-mode-alist '("_spec\\.sh\\'" . shellspec-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.shellspec\\'" . shellspec-mode))

;; Also detect by content if file starts with shellspec directives
(add-hook 'sh-mode-hook
          (lambda ()
            (when (and (string-match "_spec\\.sh\\'" (or buffer-file-name ""))
                      (not (eq major-mode 'shellspec-mode)))
              (shellspec-mode))))

(provide 'shellspec-mode)

;;; shellspec-mode.el ends here
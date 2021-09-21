(use-package gradle-mode :ensure)
;(use-package lsp-mode :ensure)
;(use-package lsp-java :ensure)
(use-package flycheck-kotlin :ensure)

(gradle-mode 1)
(setf gradle-use-gradlew t)
(setf gradle-gradlew-executable "./gradlew")

;(eval-after-load 'flycheck
;  (lambda ()
;    (flycheck-kotlin-setup)))

(defun is-gradle-project-dir (dir)
  (file-exists-p (expand-file-name "build.gradle" dir)))

(defun find-gradle-root-dir (&optional dir)
  (let ((root (locate-dominating-file (or dir default-directory) 'is-gradle-project-dir)))
    (if root
        (or (find-gradle-root-dir (file-name-directory (directory-file-name root)))
            root))))

; lsp is basically not worth it ..
;(add-hook 'kotlin-mode-hook #'lsp)
;(add-hook 'java-mode-hook #'lsp)




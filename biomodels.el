;; biomodels.el --- API for biomodels
;;; Commentary:
;;; Emacs interface to search the Biomodels database by description
;;; This package allows users to download and interact with local biomodel files
;;; DONE If the model has already been downloaded, fontify the row
;;; DONE Add new action to pop up information about model in a *model info* buffer
;;; DONE Instead of the search/download API, use the model/download API to get the xpp file

;;; Code:

(require 'request)
(require 'json)

(defvar biomodels-download-folder "~/.biomodels/"
  "Location to download and extract the model files.")

(defvar biomodels-attributes-alist 
  '((species . "--species")
   (reactions . "--reactions")
   (description . "--description"))
  "List of model attributes.")

(defvar biomodels-download-alist 
  '((xpp . "xpp")
    (png . "png")
    (pdf . "pdf")
    (sbml . "zip"))
  "List of file types available for download.")

(define-transient-command biomodels-transient ()
  "Test Transient Title"
  ["View model attributes"
    ("S" "Get Species" biomodels-get-species)
    ("R" "Get Reactions" biomodels-get-reactions)
   ;;("a" "All filetypes" biomodels-download-or-find-all-filetypes)
   ]
  ["View file types"
   ("x" "XPP" biomodels-download-or-find-xpp)
   ("v" "View model (PNG)" biomodels-download-or-find-png)
   ("s" "SBML" biomodels-download-or-find-sbml)
   ("P" "PDF" biomodels-download-or-find-pdf)
   ;;("a" "All filetypes" biomodels-download-or-find-all-filetypes)
   ])

(defun biomodels-download-or-find-file (filetype)
  "Download model under point."
  (let ((modelid (aref (tabulated-list-get-entry) 0))
        (buffer-file-coding-system 'binary)
        )
    (if (file-exists-p (concat biomodels-download-folder modelid "." filetype))
          (message "%s.%s already exists" modelid filetype)
      (progn 
        (message "%s.%s does not exist" modelid filetype)
        (write-region (request-response-data
                       (request
                         (format "https://www.ebi.ac.uk/biomodels/model/download/%s" modelid)
                         :encoding 'binary
                         :params `(("filename" . ,(format "%s.%s" modelid filetype)))
                         :headers '(("accept" . "application/octet-stream"))
                         :sync t))
                      nil (concat biomodels-download-folder modelid "." filetype))))
    (find-file-other-window (concat biomodels-download-folder modelid "." filetype))
    modelid
    ))

(defun biomodels-download-or-find-sbml ()
  ""
  (interactive)
  (let ((modelid (aref (tabulated-list-get-entry) 0))
        (buffer-file-coding-system 'binary)
        (filetype "zip")
        )
    (message "%s" (concat biomodels-download-folder modelid "." filetype))
    (if (file-exists-p (concat biomodels-download-folder modelid "." filetype))
          (message "%s.%s already exists" modelid filetype)
      (progn 
        (message "%s.%s does not exist" modelid filetype)
        (write-region (request-response-data
                       (request
                         "https://www.ebi.ac.uk/biomodels/search/download/"
                         :encoding 'binary
                         :params `(("models" . ,(format "%s" modelid)))
                         :headers '(("accept" . "application/zip"))
                         :sync t))
                      nil (concat biomodels-download-folder modelid "." filetype))))
    modelid
    ))

(defun biomodels-download-or-find-png ()
  ""
  (interactive)
  (biomodels-download-or-find-file (cdr (assoc 'png biomodels-download-alist))))

(defun biomodels-download-or-find-xpp ()
  ""
  (interactive)
  (biomodels-download-or-find-file (cdr (assoc 'xpp biomodels-download-alist))))

(defun biomodels-download-or-find-pdf ()
  ""
  (interactive)
  (biomodels-download-or-find-file (cdr (assoc 'pdf biomodels-download-alist))))

(defun biomodels-download-all-filetypes ()
  ""
  (interactive)
  (biomodels-download-or-find-sbml)
  (biomodels-download-or-find-png)
  (biomodels-download-or-find-xpp)
  (biomodels-download-or-find-pdf)
)

(defun biomodels--local-files (ext)
  "Return list of local models."
  (if (eq ext "all")
      (directory-files biomodels-download-folder nil ".zip\\|.png\\|.pdf\\|.xpp")
    (directory-files biomodels-download-folder nil ext))
)
(defconst biomodels-src-dir (file-name-directory load-file-name))

(defun biomodels-list ()
  "Make list of available files."
  (interactive)
  (let* (
         (models (biomodels--local-files ".zip"))
         (ids  (map 'list 'file-name-base models))
         (names (mapcar (lambda (x) 
                          (car (s-split "\n" (shell-command-to-string 
                                              (concat biomodels-src-dir "./sbml-interface.py --model "
                                                      biomodels-download-folder 
                                                      x 
                                                      ".zip --description"))))) ids))
         (status (mapcar (lambda (_) "Available") models))
         (columns [("ID" 20) ("Status" 15)("Name" 100)])
         (rows (mapcar* (lambda (id st nam)  `(nil [,id,st,nam])) ids status names)))
    (if (eq (length models) 0)
        (progn
          (message "No models have been downloaded yet. Search online by keyword.")
          (biomodels-search))
      (progn
        (switch-to-buffer "*biomodels*")
        (biomodels-mode)
        (setq tabulated-list-format columns)
        (setq tabulated-list-entries rows)
        (tabulated-list-init-header)
        (tabulated-list-print)))))


(defun biomodels-search ()
  "Search biomodels for searchterm.
This returns the top 50 model IDs from biomodels"
  (interactive)
  (setq searchterm (read-string "Search: "))
  (message "Searching for: %s" searchterm)
  (request
   "https://www.ebi.ac.uk/biomodels/search"
   :parser 'json-read
   :headers '(("accept" . "application/json"))
   :params `(("query" . ,(format "%s" searchterm))
             ("offset" . 0)
             ("numResults" . 50)
             ("sort" . "relevance-desc"))
   :type "GET"
   :success (cl-function
             (lambda (&key data response &allow-other-keys)
               (let* ((allmodels (assoc-default 'models data))
                      (ids (mapcar (lambda (x) (assoc-default 'id x)) allmodels))
                      (downloaded (-uniq (mapcar (lambda (x) (car (s-split "\\." x)))(biomodels--local-files "zip"))))
                      (faces (mapcar (lambda (x) (if 
                                                     (member x downloaded)
                                                     'biomodels-available-face
                                                     'biomodels-not-available-face))
                                     ids))
                      (status (mapcar (lambda (x) (if 
                                                     (member x downloaded)
                                                     "Available"
                                                     ""))
                                     ids))
                      (names (mapcar (lambda (x) (assoc-default 'name x)) allmodels))
                      (columns [("ID" 20) ("Status" 15)("Name" 100)])
                      (rows (mapcar* (lambda (id sta thisface name)  `(nil [,(propertize id 'font-lock-face thisface),(propertize sta 'font-lock-face thisface),(propertize name 'font-lock-face thisface)])) ids status
faces  names)))
                 (switch-to-buffer "*biomodels*")
                 (biomodels-mode)
                 (setq tabulated-list-format columns)
                 (setq tabulated-list-entries rows)
                 (tabulated-list-init-header)
                 (tabulated-list-print))
               ))))

(defun biomodels-get-model-attribute (attype)
  "Display species in model under point."
  (let* ((modelid (aref (tabulated-list-get-entry) 0))
         (attribute (cdr (assoc attype biomodels-attributes-alist)))
        (buf (get-buffer-create (format "*biomodels -- %s %s*" modelid attribute))))
    (if (buffer-live-p buf)
        (kill-buffer buf))
    (setq buf (get-buffer-create (format "*biomodels -- %s %s*" modelid attribute)))
    (with-current-buffer buf
      (insert 
       (shell-command-to-string (concat biomodels-src-dir "./sbml-interface.py --model " biomodels-download-folder modelid ".zip " attribute))))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (special-mode)
    ))

(defun biomodels-get-species ()
  ""
  (interactive)
  (biomodels-get-model-attribute 'species))

(defun biomodels-get-reactions ()
  ""
  (interactive)
  (biomodels-get-model-attribute 'reactions))
    
(defvar biomodels-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'biomodels-transient)
    (define-key map (kbd "s") 'biomodels-search)
    map)
  "Keymap for biomodels-mode.")

(define-derived-mode biomodels-mode tabulated-list-mode "Biomodels"
  "major mode for displaying biomodels"
  :keymap   (let ((map (make-sparse-keymap)))
              (define-key map (kbd "a") 'biomodels-transient)
              ;; (define-key map (kbd "D") 'biomodels-download-transient)
              ;; ;;    (define-key map (kbd "d") 'biomodels-download-model)
              (define-key map (kbd "s") 'biomodels-search)
              ;; (define-key map (kbd "S") 'biomodels-get-species)
              ;; (define-key map (kbd "v") 'biomodels-download-or-find-png)
              ;; (define-key map (kbd "R") 'biomodels-get-reactions)
              map))


(provide 'biomodels-mode)
;;; biomodels.el ends here

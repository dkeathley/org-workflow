;;; org-workflow.el --- Provides extra functionality for
;;; managing workflows and projects in org-mode.  It works with the
;;; org-attach and org-id systems already in place.  

;; Copyright (C) 2021 Phillip D. Keathley
;; Authors: P. Donald Keathley <dkeathley@gmail.com>
;;
;; Version: 1.0
;; Keywords: convenience, productivity
;; URL: 
;; Package-Requires: ((org))
;; EmacsWiki: FIXME

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'org)

(defun org-workflow-move-subtree-to-bottom ()
  "Move current org subtree to the end of its parent.
With prefix arg move subtree to the start of its parent."
  (interactive)
  (condition-case err
      (while t
        (funcall 'org-move-subtree-down)
		)
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg))
		 )
	   )
	 )
	)
  )


(defun org-workflow-move-subtree-to-top ()
  "Move current org subtree to the end of its parent.
With prefix arg move subtree to the start of its parent."
  (interactive)
  (condition-case err
      (while t
        (funcall 'org-move-subtree-up)
		)
    (user-error
     (let ((err-msg (cadr err)))
       (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
         (signal 'user-error (list err-msg))
		 )
	   )
	 )
	)
  )

(defun org-workflow-move-level-up ()
  "Move a subtree up one level in a workflow."
  (interactive)
  ;;Only move up/down IF you are deeper than a level 1 headline
  (unless (= (org-current-level) 1)
	(progn
	  (org-workflow-move-subtree-to-bottom)
	  (org-promote-subtree)
	  (org-move-subtree-up)
	  (org-demote-subtree)
	  )
	)
  )


(defun org-workflow-move-level-down ()
  "Move a subtree down one level in a workflow."
  (interactive)
    ;;Only move up/down IF you are deeper than a level 1 headline
  (unless (= (org-current-level) 1)
	(progn
	  (org-workflow-move-subtree-to-bottom)
	  (org-promote-subtree)
	  (org-move-subtree-down)
	  (org-demote-subtree)
	  )
	)
  )

(defun org-workflow-new-project ()
  "Fill out a new subtree item with default subtree items of NOTEBOOK and TASKS."
  (interactive)
  (let (title item-id project-folder-name proj-type folder-title)
	
	;;Get title string from user
	(setq title (read-string "Title: "))

	;;Get the folder title string. This is the core name of the
	;;attachment folder that is specified for the project.
	(setq folder-title (read-string "Folder Title: "))

	;;Insert the title heading under the current heading at point.
	(org-insert-heading-after-current)
	(insert title)
	(org-demote-subtree)

	;;Create an id for the project
	(setq item-id (org-id-get-create))
	(setq proj-type (org-entry-get nil "PROJ-TYPE" t))

	;;Build the name for the project folder.  It looks for PROJ-TYPE
	;;property that is inherited by parents.  If specified it prepends
	;;the name with that, followed by the folder name, then followed by a date
	;;stamp.  The date stamp is to help the user differentiate duplicates.
	(setq project-folder-name
		  (concat "PROJECT-ATTACHMENTS/"
				  (file-name-base (buffer-name))
				  "/"
				  proj-type "-"
				  folder-title
				  (format-time-string "-%Y-%m-%d-%H-%M")
				  )
		  )

	;;Set the attachment directory based on the ID
	(org-set-property "DIR" project-folder-name)

	;;Move point to end of the properties drawer
	(search-forward-regexp ":END:")

	;;Now insert the subtree headings
	(org-insert-subheading nil)
	(insert "Notebook")
	
	(org-insert-heading nil)
	(insert "Tasks")
	
	)
  )

(defun org-workflow-template-builder (subtitle-list)
  "Creates title from user input for a custom template. It is assumed
   that the user provides a list of subtitles in subtitle-list.  These
   are then used as the subheading titles under the project by default."

  (interactive)
  (let (item-num title project-folder-name item-id folder-title proj-type)

	;;Get title string from user
	(setq title (read-string "Title: "))

	;;Get the folder title string. This is the core name of the
	;;attachment folder that is specified for the project.
	(setq folder-title (read-string "Folder Title: "))

	;;Insert the title
	(org-insert-heading-after-current)
	(insert title)
	(org-demote-subtree)

	;;Create an id for the project
	(setq item-id (org-id-get-create))
	(setq proj-type (org-entry-get nil "PROJ-TYPE" t))


	;;Build the name for the project folder.  It looks for PROJ-TYPE
	;;property that is inherited by parents.  If specified it prepends
	;;the name with that, followed by the folder name, then followed by a date
	;;stamp.  The date stamp is to help the user differentiate duplicates.
	(setq project-folder-name
		  (concat "PROJECT-ATTACHMENTS/"
				  (file-name-base (buffer-name))
				  "/"
				  proj-type "-"
				  folder-title
				  (format-time-string "-%Y-%m-%d-%H-%M")
				  )
		  )
	
	;;Set the attachment directory property that will be inerited
	;;down to any further generated sub-items.
	(org-set-property "DIR" project-folder-name)

	;;Move point to end of the properties drawer
	(search-forward-regexp ":END:")

	;;Variable to track the number of items looped through
	;; -- this is b/c you need to differentiate between the
	;; -- first item as a subheading, and the next items as
	;; -- headings.  
	(setq item-num 0)
	
	;;Loop through all subtitles provided to construct
	;;the subtree:
	(dolist (subtitle subtitle-list)
	  (if (= item-num 0)
		  (progn
			;;Insert subheading on fist time
			(org-insert-subheading nil)
			(insert subtitle)
			(setq item-num (+ item-num 1))
			)
		(progn
		  ;;Insert heading after that to stay in same level
		  (org-insert-heading nil)
		  (insert subtitle)
		  (setq item-num (+ item-num 1))
		  )
		)
	  )
	)
  )

(defun org-workflow-convert-to-project ()
  "Ensures that the :ID: and :DIR: properties are set to be consistent
   with automatically-generated project headlines.  "
  
  (interactive)

  (let (item-id project-folder-name)

	;;Create an id for the project
	(setq item-id (org-id-get-create))
	(setq project-folder-name (concat "PROJECT-ATTACHMENTS/" item-id))

	;;Set the attachment directory based on the ID
	(org-set-property "DIR" project-folder-name)

	)

  )

(provide 'org-workflow)


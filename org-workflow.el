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
  "Move current org subtree to the end of its parent."
  (interactive)

  ;;Use org-reveal to expand out all sub-tree headings under the current master heading.
  ;;This prevents any strange behavior from folded subtrees.
  (org-reveal)
  
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
  "Move current org subtree to the top of its parent."

  (interactive)

  ;;Use org-reveal to expand out all sub-tree headings under the current master heading.
  ;;This prevents any strange behavior from folded subtrees.
  (org-reveal)
  
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
  "Move a subtree up one level to the previous stage in a workflow."
  (interactive)
  ;;Only move up/down IF you are deeper than a level 1 headline
  (unless (= (org-current-level) 1)
	(progn
	  (org-workflow-move-subtree-to-bottom)
	  (org-promote-subtree)
	  (org-reveal)
	  (org-move-subtree-up)
	  (org-demote-subtree)
	  (org-reveal)
	  (org-workflow-move-subtree-to-top)
	  )
	)
  )


(defun org-workflow-move-level-down ()
  "Move a subtree down one level to the next stage in a workflow."
  (interactive)
    ;;Only move up/down IF you are deeper than a level 1 headline
  (unless (= (org-current-level) 1)
	(progn
	  (org-workflow-move-subtree-to-bottom)
	  (org-promote-subtree)
	  (org-reveal)
	  (org-move-subtree-down)
	  (org-demote-subtree)
	  (org-reveal)
	  (org-workflow-move-subtree-to-top)
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
	(setq suggestion (replace-regexp-in-string "[\s]" "-" title))
	(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
	(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
	(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
	(setq folder-title (read-string "Folder Title: " suggestion))

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

  (let (item-id
		project-folder-name
		proj-type
		folder-title
		current-dir
		want-cancel
		)
	
	;;Create an id for the project
	(setq item-id (org-id-get-create))
	(setq proj-type (org-entry-get nil "PROJ-TYPE" t))

	;;These are the want-XX flags.
	;; -- Default to false in general.
	(setq want-cancel nil)
	
	;;Let's get the current directory at point
	(setq current-dir (org-entry-get (point) "DIR"))
	(when current-dir
		;;What we want to do is warn the user and ask if they want
		;;to overwrite the current directory
		
	  (let (yn-response go-again warning-message)
		(setq go-again t)
		
		(setq warning-message
			  (concat "Note: The existing directory will be moved if you proceed. Is this OK? \n"
					  "Current DIR: "
					  current-dir
					  "\n[Y/y]es/[N/n]o: "
					  )
			  )

		;;Setup a loop until the user presses one of the
		;;accepted entries Y/y, N/n, or C/c
		(while go-again

		  ;;Get the entry from the user, displaying the appropriate
		  ;;warning message about each choice
		  (setq yn-response
				(read-string warning-message)
				)

		  ;;Check the state of the user response
		  (if (or (string-equal yn-response "N") (string-equal yn-response "n"))

			  ;;If no selected you want to break the loop and do nothing.
			  ;; -- Do not rename the directory.
			  ;; -- Don't change anything in the file.
			  ;;User decides to cancel -- escape
			  (progn
				;;Cancel response
				(setq go-again nil) ;;Ensure go-again is nil to terminate loop
				(message "Operation canceled.  Nothing changed.")
				(setq want-cancel t)
				)
			
			;; OK -- not cancel, so let's move forward and check if Y/y.  
			;; We will need to set the want-XX flags accordingly depending on the response.
			(progn
			  
			  
			  ;;Now check if the user pressed Y or N for moving the existing directory:
			  (when (or (string-equal yn-response "Y") (string-equal yn-response "y"))
				  
				;;Response: Yes.  Move/rename the directory on disk if it exists.
				;; -- Be sure to want-move to true
					(setq go-again nil) ;;Ensure go-again is nil to terminate loop
				  
					)
		  
			  )
			)

		  ;;Now, if go-again has not been set to nil, we know the user did not
		  ;;enter one of the accepted responses.  We need to warn again and go back to start.
		  (when go-again
			(setq warning-message
				  (concat "Enter only Y/y or N/n.\n"
						  "Note: The existing directory will be moved if you proceed. Is this OK? \n"
						  "Current DIR: "
						  current-dir
						  "\n[Y/y]es/[N/n]o:"
						  )
				  )
			)

		  

		  )
		)
	  )
	  
	;;Now that we are sure what the user wants -- let's move forward with all actions.
	;; -- All want-XX flags should be set appropriately.
	;; -- We just need to run through with all of the actions needed.

	;;Unless the users want to cancel, move forward:
	(unless want-cancel
	
	  ;;Get the folder title string. This is the core name of the
	  ;;attachment folder that is specified for the project.




	  ;; If the  current directory exists, then take that name as a starting point:
	  (when current-dir
		;; Give the default suggestion as the current folder, removing the directory prefix...
		(setq dir-prefix (concat "PROJECT-ATTACHMENTS/"
								 (file-name-base (buffer-name))
								 "/"
								 proj-type "-")
			  )
		(setq folder-title (read-string "Folder Title: " (replace-regexp-in-string dir-prefix "" current-dir)))
		)

	  ;; If the current directory does not exist, then we need to generate a new one...
	  ;; It is generally more efficient to start with the heading title removing all spaces
	  ;; (org-get-heading 1 1 1 1) does this, removing all unneeded metadata such as tags, comments, etc...
	  ;; Spaces are then replaced with a dash "-"
	  (unless current-dir
		(setq suggestion (replace-regexp-in-string "[\s]" "-" (org-get-heading 1 1 1 1)))
		(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
		(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
		(setq suggestion (replace-regexp-in-string "--" "-" suggestion))
		(setq folder-title (read-string "Folder Title: " suggestion))
		)

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
					)
			)
	  
	  ;;Set the attachment directory based on the ID
	  (org-set-property "DIR" project-folder-name)

	  ;;Check if directory exists... if so then rename it.
	  (when current-dir
		(when (file-exists-p current-dir)
		  (rename-file current-dir project-folder-name)
		  )
		)
	  )
	)
  )


(defun org-workflow-change-project-directory ()
  "This is actually an overloaded function call to org-workflow-convert-to-project.
   It is provided for convenience."

  (interactive)
  
  (org-workflow-convert-to-project)

  )
  

(provide 'org-workflow)


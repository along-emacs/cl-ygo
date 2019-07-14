# cl-ygo
Common Lisp version of YGO engine

## Features:
1. parse ydk file: ```(parse-deck "deck name")``` \\
   For example, format of ydk file:
   ```
   #created by ...
   #main
   card id 1
   card id 2
   #extra
   card id 3
   card id 4
   !side
   card id 5
   ```
  Will return a list like:
	```commonlisp
	'(:deck (card id 1 card id 2 )
	  :extra (card id 3 card id 4) :side (card id 5))
	```
2. init deck from ydk file: ```(init-deck "deck name")```
3. 

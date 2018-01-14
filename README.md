# Hoogle example UI built with brick

bhoogle is a very simple wrapper over hoogle and a demo brick application

![ui](ui.png)


## Setup
 - Make sure you have a local hoogle database created
 - If you don't already, then
   1. Install hoogle (e.g. ```stack install hoogle```)
   1. Generate the default database (```hoogle generate```)

## Usage
 1. Enter a search in the "type" edit box
 1. Press enter to search: focus goes directly to the results list
 1. Or press tab to search and focus will go to the "text" edit box
 1. You can then filter the results by typing in the "text" edit box, any result containing the sub-string typed will be shown
 1. Navigate the results by using arrow or vi (hjkl) keys
 1. Pressing **'s'** in the results list will toggle the sort order
 1. Escape to exit

## Notes
 1. I'm using the latest brick at the time of writing this (0.33). See "extra-deps" in stack.yaml

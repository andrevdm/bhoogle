# Hoogle terminal GUI.

**bhoogle** is a simple terminal GUI wrapper over [hoogle](https://hackage.haskell.org/package/hoogle). 


![ui](http://www.andrevdm.com/images/bhoogle.png)


## Setup
 - Make sure you have a local hoogle database created
 - If you don't already, then
   1. Install hoogle (e.g. ```stack install hoogle``` or ```cabal install hoogle```)
   1. Generate the default database (```hoogle generate```)

## Usage
 1. Enter a search in the "type" edit box
 1. Press enter to search: focus goes directly to the results list
 1. Or press tab to search and focus will go to the "text" edit box
 1. You can then filter the results by typing in the "text" edit box, any result containing the sub-string typed will be shown
 1. Navigate the results by using arrow or vi (hjkl) keys
 1. Pressing **'s'** in the results list will toggle the sort order
 1. Escape to exit
 1. Search-ahead is enable for any type search longer than ~3 characters
 1. When a result is selected `p` yanks the package name
 1. When a result is selected `m` yanks the module name


## Settings

Location: ~/.config/bhoogle/bhoogle.conf 

Eg:

    yank=xclip
    yankArgs=-selection c


Note that the version described in the [blog](http://www.andrevdm.com/posts/2018-01-15-bhoogle.html) is on the [blog](https://github.com/andrevdm/bhoogle/tree/blog) branch.


# emphatic 0.1.6.9014  2024-05-26

* Removed `hl_mat()` and associated vignettes.
    * function is not exported for now as the selection process is too 
      difficult to describe well. Needs a rethink
* Refactored colour handling for `hl_grep()` and `hl_diff()`
* Settled on `text`/`fill` nomenclature
* Refactored arguments to `hl()` for more clarity
* Added latex output so Quarto/Rmd will show emphatic objects when 
  rendered to PDF
* Remove `scale_mode` argument to `hl()`
* Support for output to `typst` in quarto docs
    * Output to raw blocks in typst
    * only character to be escaped is the backtick
* latex rendering now has better whitespace at start of line
* SVG anim support
* Excel export
* Refactor options for hl_grep and hl_diff
* html, typst, svg and latex output all now support font size argument

# emphatic 0.1.6  2024-04-27

* Consolidated `hl_grep()` variants into single function
* `hl_diff()` now operates on any object
* `hl_vec()` removed
* `hl_mat()`    
    * now has a `byrow()` argument
* Setting options has been totally overhauled


# emphatic 0.1.5  2023-08-30

* Added `hl_diff()` to highlight differences between two strings.
* Added functions for highlighting the results of a `grep()`
    * `hl_grep()`
    * `hl_str()`
    * `hl_character()`
    * `hl_print()`
    * `hl_deparse()`

# emphatic 0.1.4  2020-11-13

* Fix for rendering of logical atomic vectors

# emphatic 0.1.3  2020-11-12

* Added support for highlighting atomic vectors
* Added ability to add legends when using scales to assign colours

# emphatic 0.1.2  2020-11-11

* Added support for tibbles (by internally converting to data.frames 
during output)

# emphatic 0.1.1  2020-11-01

* Added bespoke html rendering. Removed dependency on `fansi`


# emphatic 0.1.0  2020-10-30

* Initial release

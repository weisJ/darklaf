### Visual changes
- `JToolBar`s now have default padding. 37b8e5e0eb4a4e5aa36982f8ad8bc7dcab6ac7f4

## Behavioural changes
- If scrollbar hiding is enabled (through `ScrollBar.macos.hideScrollBar`) scrolling will now reveal the scrollbar.
- The labels for the current RGB (or HSB/HSL respectively) values in `SmallColorChooser` can now be edited directly.
  `SmallColorChooser` is used for `PopupColorChooser`, `QuickColorChooser` and as the default table cell editor for `Color.class`. efd996ce8a3159790c270fedb8cba9b9526a5ca4

### API Changes
- Added a default color palette which is available with each theme. The colors are adjusted to fit with in with the style of the given theme. 9b49c7af87aa237c676f926038a284ec3c241aea b1cebab636261187d6e6698880bead8f99f0d7f1
  The following colors are available:
  ````
  palette.yellow
  palette.orange
  palette.red
  palette.pink
  palette.purple
  palette.indigo
  palette.blue
  palette.teal
  palette.cyan
  palette.green
  palette.lime
  palette.forest
  palette.brown
  palette.gray
  ````
- Introduced new convenience methods to `DarkBorders`for creating borders. 05a6b54375b3f79794766a3e1a0e0f553b077501
- For `CustomThemedIcon` the defaults for context properties (i.e. the properties which are usually resolved against the 
  current `UIDefault`s) can now be changed. 2c55ad4f119439533ce4912d8c30bf89fdf07026
- SVG icon properties can now reference other properties through the `%` prefix (Properties will be resolved up to 5 levels deep). 2c55ad4f119439533ce4912d8c30bf89fdf07026
- Added the `ButtonConstants.KEY_ARC_MULTIPLIER` client property for `AbstractButton` to specify the multiplier of the corner radius. 
- Added `Theme#baseThemeOf(Theme)`, which returns the given theme stripped from font and accent color changes. 6170f72a3e7f3ba1428cb2c2a168cbd0f9b2134f
- 

### New components
- New `CloseButton` as a convenience class for creating buttons with a "Close Button" visual. 2050a149de975f5f1bd0826ea2b4c116e1259b84
- Added `IconEditorPanel` and `IconEditor` which can be used to edit colors of themed svg icons. 0b753bea5da6e4f8c73cf4cbfa08eb2bd2729cf2

### Other changes
- `Darklaf#getID` now always returns `"darklaf"`. 25554e9158b67172b302545ea0cbcb07d71bb83b
- Most icons now specify fallback values for their colors which enables the usage of the outside of darklaf. 3ec5dfd6ebb89d203ad0af6ed16b3c18c23605d2

### Addressed issues
- The auto generated property tables have incorrect values. b6ea136a7412916fb8958c6d6628ce219a34b8b1
- The background bleeds through between the slider border and focus ring. 9d8f043c2c0022df09abcf5a59ab9e45214116f3
- NPE while uninstalling the UI of `JEditorPane`. f0bcd89a67b3efc048f64d394ad55e459021a204
- Settings changed through `ThemeSettings` can visually enable functions which aren't supported on the current OS. acd0f36067bd477d608bc86d941885d5f0f1074f

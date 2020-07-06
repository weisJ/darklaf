# Changes since last release

### New in this release
- Using non-default fonts with `JTextField` will now result in more visually centered text.
- Opacity values for `CustomThemedIcon` can now be specified using `float` or `double` values as
  opposed to `int` percentages.
- If properties aren't present in the property map for a `CustomThemedIcon` they are now searched
  for in the `UIDefualts`
- SVG color definitions can now specify fallback keys for opacity values.
- Accessing `ThemeSettings` will no longer instantiate the settings panel unnecessarily. 
- Improved visibility of which accent/selection color is selected settings panel.
- Added `LafManager#getLogLevel` method to fetch current log level.
- Decorations can now be toggled seamlessly [Windows].
- The unified menubar on Windows can now be toggled by setting the `DarkRootPaneUI.KEY_UNIFIED_MENUBAR` property on the
  frames `JRootPane`.
- Added `JToggleButtonList` component. A more general version of `JCheckBoxList`.
- When hovering over the row of a `JTree` that isn't completely visible the full row will now be made visible using a popup.
- Tooltips on components that aren't buttons now have the `plain` tooltip style by default.

### Addressed issues
- Default text doesn't have the same font as the text component it's painted on. c0756ed058a63b4ac5cf4f45eb767a691709bcdc
- `ColoredRadioButton` patches color of icon unnecessarily often. 6ff0ffc799dfb2a304d5bd4ddfe679059143226b
- Minimize butten in title bar won't loose rollover status [Windows]. 1e3693b3a2f256df4d3e14a6cae8d18d4b359eea
- Fast scrolling doesn't work when mouse is over the scrollbar of an `OverlayScrollPane`. c98b7a4f12f2b22319e4c9f84439e0d6687bc909
- Heavyweight popups have an incorrect location after frame is moved to second monitor with possibly different scaling mode. af00f15f6402c95b7127f30ca15769a3ae4f6556
- Rounded and extended selection isn't painted correctly when text wrapping is enabled. 860680dc9374d3b5b37370f842c0cdab7f81b044
- Icons of buttons aren't properly clipped. 87a5295f06461532bfe659a1c2837a179f573bc0
- Selected and disabled `JToggleButton`s use the wrong foreground color. #191 6d4c3bc6df1a7a944cd40cf87ec15102f51759ec
- After switching to a different Laf the incorrect `PopupFactory` is used. 1c3d33af8bfdb00eed271083fda6d2fddb8da5e6
- Laf isn't installed if theme hasn't changed but current laf isn't Darklaf when using `LafManager#install(Theme)`. 9dd137120f93b46598e7257091e79c627d6f7043

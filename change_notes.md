# Changes since last release

### New in this release
- Localisation for titlebar buttons.
- Added option to make tabs closable on a per tab basis for `ClosableTabbedPae`.
- Added utility method to create themed frame icons that change depending on the
  current theme:
  ````java
  window.setIconImage(IconLoader.createFrameIcon(icon, window));
  ````
- The value of `ButtonConstants.KEY_SQUARE` no longer affects the arc size of buttons.
- If a button specifies the `ButtonConstants.VARIANT_BORDERLESS` style it no longer has a default margin of the focus border size.
  Use `AbstractButton#setMargin` to add back a margin.
- Added option to specify a custom `JScrollPane` for use with `OverlayScrollPane`.
- Color improvements and accent/selection color support for OneDark theme.
- Selection color support for Solarized themes.
- Added preset selection colors corresponding to each accent color.
- Try to keep original pointer location when aligning tooltip.
- Added warning for when a themed icon isn't loaded as themed.
- Added warning for when a non-themed icon is loaded as themed.
- Improved sub-pixel antialiasing for non-opaque windows on Windows. (Relevant [JDK issue](https://bugs.openjdk.java.net/browse/JDK-8215980?attachmentOrder=desc))
  - Before:
  - After:

### Addressed issues
- NPE when navigating tree in some scenarios. #182  198e5fbe977b2ea3defe1cb1f1b961409950bc7f
- Default buttons loose their default status after the theme has changed. 333f8a4b905643cd01b19ad9cc804683b0af882b
- NPE when calculating baseline for text fields. #184 dec0c8eba52480d35ed30454bbb3a4ec2811774b
- `DarkTextBorder` on a component which isn't a `JTextField` has incorrect insets. 133bed00c4198f95524dc14a8094a3b47ef82940  
- Unfocused cells have the wrong foreground color. 4cba70f327f53ddfd03214d0df0f37ff0137f46a
- `JList` incorrectly doesn't fully paint rows with striped background. e11da551d7451def7bb32ef558f21bbfbcf74e8b
- `PopupColorChooser` isn't displayed. c62763702edb646766f64a0c0e183be358e69f8e
- Foreground color for selection color has incorrect value. c133c515f94a493aef2eedd3dc0db3269ade7fc6
- Popups show up at the wrong location after moving the frame to a screen with a different `GraphicsConfiguration`. 995da5a2325fad4e3873388a215bbb345a779658 
- The value of `ComboBox.selectionBackground` isn't respected. 6b9fa9f1596bbf90914d7008c4d49fc43f3efb94
- `JTree` doesn't paint the full background if repainted area lies outside of the row bounds. 4e5fe2a6faea05e9315bc1e9b30d7cec92ce9e6b
- Tooltips have an incorrect position in some scenarios. a42f4bbbaa1c8d245efc1b95ad56e3c9aec6701d

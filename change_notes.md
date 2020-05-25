# Changes since last release

### New in this release
- Support for insert text mode.
- Text cursor will now show directional hints for bidi text.
- Added cut/copy/paste context menu for text components.
- Balloons tooltips are now enabled by default for buttons.
- Improved html style sheet.
- On Windows (Vista and later) darklaf will now use the system font.
- On macOS Catalina the default font will now be `Helvetica Neue` due to kerning issues with the `San Francisco` font.
- Better interoperability with custom cell renderers.
- Improved selection painting.
  - Added property to control whether the selection is extended to the end of the text component.
    ````java
    textComp.putClientProperty(DarkTextUI.KEY_EXTEND_LINE_SELECTION, enabled);
    ````
- Performance improvements.
- New scrollbar appearance on macOS that resembles the system appearance more closely.
- Improved slider thumb appearance.
- Added rounded button variant.
  ````java
  button.putClientProperty(DarkButtonUI.KEY_ROUND, true);
  ````
- Added warning variant for text components.
  ````java
  textComp.putClientProperty(DarkTextUI.KEY_HAS_WARNING, true);
  ````

  ![Text Warning](https://github.com/weisJ/darklaf/wiki/text_warning.png)
- Added `HelpButton` component.

  ![Help Button](https://github.com/weisJ/darklaf/wiki/help_button.png)
  

### Addressed issues
- Disabled tables/trees/lists don't paint the cells disabled. dcf86461aeb7770bcf25f5c75f7f7c0c35dadab3
- Non standard cares shift text when at the end of the document. 5bcda602f64f741842a95a52f3d31978c2673ffe
- Visuals of other tex components may be visible outside of border. aaa3a0849ea9235954f3d95cdcdcab5c871bbca2
- TableHeader doesn't scroll along when TableModel is set after creating the table. #163 d727dbcfa3e8fc23287adc010c89ef4065d8a8c4
- Table doesn't use the `Object.class` renderer when set. #164 1e1f02f24197a55c349423f8a69be49c05b870ee
- Layout of `QuickColorChooser` has undesirable resize behaviour. #165 673eb7717222f7289279a81370138dbf373c9d5f
- Incorrect popup location when using multiple monitors with different resolutions. #162 59e3c8e5f07479a1a717e42d48b82d763cf8f8cc
- Poor performance when using bidirectional text with non zero margins. #167 252885df6fa18044c215361cbbe200cdf3358cf5
- Text isn't painted when using the toggle button slider variant. 0d1f2913dd25d5a684fa2567c37a94b549f030b0
- Kerning issues on macOS Catalina. #128 d3fd5dedabe11d84685e593a03b1941b8ea56836

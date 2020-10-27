### Visual changes
- Updated the border of `JTextField, JFormattedTextField, JPasswordField` to the new rectangular design found in IntelliJ. 7ca292cc873aaa7725daf1dfe2b69d67ac718271
- Added animations to the slider variant of `JToggleButton`. 331c5504c3db2091ddaa27a8349cc16f312864ca
  ![Animated Slider]()
- Added on/off labels to slier variant of `JToggleButton` on the two high contrast themes. 331c5504c3db2091ddaa27a8349cc16f312864ca
  ![SLider labels]()
  This can be enabled for other themes through the `ToggleButton.showSliderHints` property.
- Added animations to the `JTree` control icons. d6e89f86e0e81704284e47e13765bb388b271d74
  ![Tree control animations]()
- If the cell of a `JTable` with a `boolean` value isn't editable this is now reflected in the renderer. 1431f0e53a8f91439e9d2c1daf16042fabed7bf4
  ![Table boolean rendeer]()  
- Added an icon to `JColorChooser`.
- Updated background behind the arrow(s) on `JComboBox` (`JSpinner`) to match IntelliJ. 578504ad189edda9c98ea332972548a53c121e4d
- The width of the popup of `JComboBox` is no longer bounded by the width of the `JComboBox`. 175fbb78e444c10674a981e2d35d69a83b8dfbc1
- `JTable#setFillsViewportHeight` now has a default value of `true` to ensure grid lines always fill the complete viewport 296b773ebdd8b38615b85bd9a9840551ed4e2139.

## Behavioural changes
- The value of a `JSlider` can now be changes through mouse wheel rotation. bb276b4193eccb248d027b9d3ee74abf1ba33ec9
- THe value of a `JSpinner` can now be changes through mouse wheel rotation. 02e431481ac10f0050a4f41cd9a48417c69e079e

### Api Changes
- Added client property on `JRootPane` to hide the custom titlebar. #211 50284319b7f218a6ec47dd7b1f4df8d13bf7aa59
  ````java
    rootPane.putClientProperty(DarkRootPaneUI.HIDE_TITLEBAR, true);
  ````
- Added system property `darklaf.animations` to disable all animations.
  ````java
  System.setProperty("darklaf.animations", "false");
  ````
  Single animations can also be disabled:
  - `JToggleButton` slider variant through the ui property `ToggleButton.animated`.
  - `JScrollBar` through the ui property `ScrollBar.animated`.
  - `JTree` control icons through the ui property `Tree.iconAnimations`.
- Added ui property to disable colored title bars: `"macos.coloredTitleBar`.
  If disabled the titlebar will be either the light or dark native titlebar based on the current theme. f9f4e490cff4f48607165fc8d1b9e31efc69d257
- Added `IconLoader#createDerivedIcon(Icon,int,int)` as a utility method to rescale icons. 52fc48d52c59f8c809622a4158d14bab582b89e7
- Added `JTabFrame#isTabSelected(Alignment,int)` to check whether a tab is currently selected. 4d230608afc732c4d10d3b8880f7f9c3a17fd3c6

### New components
- Added `JSplitButton` which opens a popup menu if no action listener is installed or has a separate drop-down menu otherwise. ec14c8c199f3d69680f7517f8ede6c56954f45d0
  ![Todo]()

### Other changes
- Custom background colors on `JComboBox, JSpinner` are now respected. 52ea49dc34b9402ed51c72d3f31c11ab8439e0ad, e1d0c3d9df95bafd374088209c69f0dfe9b4102f
- The annotations processor for `@SynthesiseLaf` is now incremental. 11c74eaa4efa76fdca3cb27c23248ea86a35675c

### Addressed issues
- LaF doesn't get installed if changed through `ThemeSettings` if the default theme is requested. e2aabdd378d7097260fbf0fb2563d2bc31cd0331
- `JTabFrame` split positions can't be changed before the component is shown. 67fdf5e6d79c173d54bba5d14b67c1e63980f66e, 787f3f6f712e9887f74ef65ccb91cb278fd1287c
- Background color of `JScrollBar` isn't updated on theme change. 060a46334877ebc308fe0270e6d9319d8dd196b4
- Custom title bar has a gab between window buttons and frame content [Windows]. fd50bb8cd294d591852faf075c340335cb2cd734, 00c6f0958917d27442018f0920012904681cf0db
- Popups of `JTabFrame` can't be closed through keyboard shortcut if the content isn't opaque. 194171ac9837ae4d6b59d508f9b2ac0eac54647d
- `LogFormatter` and `LogHandler` exist in two places. #212 8de4f56c00b6752d6306c815d9bbd63de924f825
- Editor component of `JSpinner` doesn't have an empty border after theme change. 9c265a8cd4aaa62db475e81e36bcb3c838cf23f1
- `JSpinner` doesn't have the correct background specified by the theme. 59efefe7a9aefd051aaf6702343f68381050230e
- Tree controls of parent node aren't made visible when navigating the tree through keyboard shortcuts. 67407b3e8b12ec52addf498dcf83f6cdd6bb4621
- Boolean values in `JTable` aren't updated as soon as the value in the editor changes. #213 31b16fbd9e1132da0c5f70289899c927281fd83d
- The expanded tree control of a disabled `JTree` is displayed as collapsed. 1431f0e53a8f91439e9d2c1daf16042fabed7bf4

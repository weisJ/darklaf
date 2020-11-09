### Visual changes
- The knob of the toggle button slider variant is now flush with the button border. 75fa2239112d403b2c77bcaed42757fc9a703d2c
- Made button shadow lighter in the `SolarizedLightTheme`. 7aedb3a38e6dd834108c8b8a357f7ac84a283fa3
- Buttons will now have the same height as combo boxes (and spinners). 7aedb3a38e6dd834108c8b8a357f7ac84a283fa3
- `SolarizedLightTheme` now supports custom accent colors. 0bd0de485a43ce0d568b55f5fac09739262c7b64
- Improved visibility for rollover/click button colors for dark high contrast theme. 8b283ef5d1ab6bf646c4b290ce8e816d11a5d394

## Behavioural changes
- When using a custom background color<sup>[1](#customValue)</sup> for a combobox renderer the combobox will adapt the background color. 7f6681e33f42d75516efac30657417108a2a871d
  - If the custom background is only intended for the popup then one can check whether the `index` is `-1` in `ListCellRenderer#getListCellRendererComponent` which will be the case
    when painting the value in the combobox.
- Custom fonts<sup>[1](#customValue)</sup> for combobox renderers will no longer be overwritten by the combobox font. 7f6681e33f42d75516efac30657417108a2a871d
- When using a custom background color<sup>[1](#customValue)</sup> for a spinner editor the spinner will adapt the background color. 04a9f171351f2fad15284837c8e9257448f11996
- If the height of a combobox isn't large enough for the current value the margins will be reduced to try to fit the value inside. 4f29251aae457d95c28fa72fd3aa55d9a61d04a5

### Api Changes
- Added `TooltipContext#setLayoutTarget` which can be used to layout the tooltip relative to a different component than the tooltip is dispatched from.
- Reintroduced `textIcon*` properties with fixed color values. #126 fa854b2dea8f56d13f8d6072fcece6fd3c428ead
- Added `ScrollBarConstants#KEY_HIGHLIGHT_ON_SCROLL` client property to enable scrollbar highlighting when scrolling.
  By default, this value gets initialised to the value of the `ScrollBar.highlightOnScroll` property of the `UIManager`
  (which is `false` by default). 6f6c8dbc90742fc5d33e7f96ddbba75ae1200e44
- Added `ScrollBar.macos.hideScrollBar` property to `UIManager` which determines whether the scrollbar should be hidden when
  it's not used currently (i.e. has no mouse interaction). By default, this value is `false`. 6f6c8dbc90742fc5d33e7f96ddbba75ae1200e44  
- Added `HelpButton#setUseColoredIcon` to toggle between a colored and non-colored help icon. 81e37c3e1fe78c9ff611c32bf23023cb370ba57d

### New components

### Other changes

### Addressed issues
- NPE when using `JScrollBar` on macOS. ([Kohei Sakurai](https://github.com/ppp-kohe)) 7ee01c45d512a56d8b92be1a2f8272aee79988c5
- `JComboBox` has incorrect preferred size. 9459c75473e6b9aa5671e72ad1d3d8b540bd971b
- NPE on mouse-over when using `JToolTip` as a child of another component. 897766c50475d861dbb0ef33825b820101aa9d0f
- Tooltip fade-in animation is started if used as child of another component. 8b1b5549d464c37b3b1237e527f66c1c4cb83e1a
- Text is truncated when editing table. 6df91b7aac8b5260400c65dd0ca391c035c55478
- Tables don't use designated color renderer and editor. b1ca895e55dd7453adfbb08b766c1267beda2e08
- Popup for combobox is too large. 9b5aebca48ec54307910bcfde079f7cbb0d11075
- Focus border of slider toggle button is clipped. d6d500d067b28294380bfb1b37b80598995f07f0
- Button looses rollover state after it has been pressed. 5709f5f1132f6ae67f641bc374991596d6b236ab
- Dropdown button of `JSplitButton` disappears after the theme has been changed. 0aad0104c17da4d42b6ddf36ee49ff63a3a5ce29
- Buttons who qualify for automatic borderless conversion and explicitly opt-in may display an incorrect background area. 55697f6411a04e40b53505d32420de377c822b3f
- Arrow buttons (as used in `JComboBox`, `JSpinner` and `JSplitButton`) can't be pressed near the edge. 52059aa4a1294a7cf6cf55865f6d54d15b8a69ca
- Custom icons for a `DefaultTreeCellRenderer` are getting ignored. 82331b08d1793b4e06cb129d07c779845cb2c83d
- Snapped values of `JSlider` slightly differ from exact value. 2442196058f5ecd83d125f0957a421b25cb0b4b4

<a name="customValue">1</a>: Not `null` and not of type `UIResource`. 897766c50475d861dbb0ef33825b820101aa9d0f

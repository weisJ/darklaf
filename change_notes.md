### Visual changes
- The knob of the toggle button slider variant is now flush with the button border. 75fa2239112d403b2c77bcaed42757fc9a703d2c

## Behavioural changes
- When using a custom background color<sup>[1](#customValue)</sup> for a combobox renderer the combobox will adapt the background color. 7f6681e33f42d75516efac30657417108a2a871d
- Custom fonts<sup>[1](#customValue)</sup> for combobox renderers will no longer be overwritten by the combobox font. 7f6681e33f42d75516efac30657417108a2a871d
- When using a custom background color<sup>[1](#customValue)</sup> for a spinner editor the spinner will adapt the background color. 04a9f171351f2fad15284837c8e9257448f11996

### Api Changes

### New components

### Other changes

### Addressed issues
- NPE when using `JScrollBar` on macOS. ([Kohei Sakurai](https://github.com/ppp-kohe)) 7ee01c45d512a56d8b92be1a2f8272aee79988c5
- `JComboBox` has incorrect preferred size. 9459c75473e6b9aa5671e72ad1d3d8b540bd971b

<a name="customValue">1</a>: Not `null` and not of type `UIResource`.

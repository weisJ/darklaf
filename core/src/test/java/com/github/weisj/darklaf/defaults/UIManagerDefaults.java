/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.defaults;

/*
 * This programs uses the information found in the UIManager to create a table of key/value pairs
 * for each Swing component.
 */

import java.awt.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.DefaultTableModel;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.table.renderer.DarkColorTableCellRendererEditor;

public class UIManagerDefaults extends BaseComponentDemo implements ItemListener {
    private static final String[] COLUMN_NAMES = {"Key", "Value", "Sample"};
    private static String selectedItem;
    private final TreeMap<String, TreeMap<String, Object>> items = new TreeMap<>();
    private final HashMap<String, DefaultTableModel> models = new HashMap<>();
    private JComboBox<String> comboBox;
    private JRadioButton byComponent;
    private JTable table;

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new UIManagerDefaults());
    }

    @Override
    public JComponent createComponent() {
        JComponent contentPane = new JPanel(new BorderLayout());
        createPreviewPanel();
        contentPane.add(createSelectionPanel(), BorderLayout.NORTH);
        contentPane.add(createPreviewPanel(), BorderLayout.CENTER);
        resetComponents();
        return contentPane;
    }

    @Override
    public String getName() {
        return "UIManager Defaults";
    }

    /*
     * This panel is added to the North of the content pane
     */
    private JComponent createSelectionPanel() {
        comboBox = new JComboBox<>();

        final JLabel label = new JLabel("Select Item:");
        label.setDisplayedMnemonic('S');
        label.setLabelFor(comboBox);

        ItemListener itemListener = e -> {
            selectedItem = null;
            if (table.isEditing()) {
                table.getCellEditor().stopCellEditing();
            }
            table.clearSelection();
            resetComponents();
        };

        byComponent = new JRadioButton("By Component", true);
        byComponent.setMnemonic('C');
        byComponent.addItemListener(itemListener);

        final JRadioButton byValueType = new JRadioButton("By Value Type");
        byValueType.setMnemonic('V');
        byValueType.addItemListener(itemListener);

        final ButtonGroup group = new ButtonGroup();
        group.add(byComponent);
        group.add(byValueType);

        final JPanel panel = new JPanel();
        panel.setBorder(new EmptyBorder(15, 0, 15, 0));
        panel.add(label);
        panel.add(comboBox);
        panel.add(byComponent);
        panel.add(byValueType);
        return panel;
    }

    private JComponent createPreviewPanel() {
        final DefaultTableModel model = new DefaultTableModel(COLUMN_NAMES, 0);
        table = new JTable(model) {
            @Override
            public boolean isCellEditable(final int row, final int column) {
                return false;
            }
        };
        table.setAutoCreateColumnsFromModel(false);
        table.setShowHorizontalLines(false);
        table.getColumnModel().getColumn(0).setPreferredWidth(250);
        table.getColumnModel().getColumn(1).setPreferredWidth(500);

        table.getColumnModel().getColumn(2).setPreferredWidth(100);
        table.getColumnModel().getColumn(2).setCellRenderer(new SampleRenderer());
        table.getColumnModel().getColumn(2).setCellEditor(new DarkColorTableCellRendererEditor());

        final Dimension d = table.getPreferredSize();
        d.height = 350;
        table.setPreferredScrollableViewportSize(d);

        return new OverlayScrollPane(table);
    }

    /*
     * When the LAF is changed we need to reset the content pane
     */
    public void resetComponents() {
        items.clear();
        models.clear();
        ((DefaultTableModel) table.getModel()).setRowCount(0);

        buildItemsMap();
        final Vector<String> comboBoxItems = new Vector<>(50);
        for (final Object key : items.keySet()) {
            comboBoxItems.add((String) key);
        }

        comboBox.removeItemListener(this);
        comboBox.setModel(new DefaultComboBoxModel<>(comboBoxItems));
        comboBox.setSelectedIndex(-1);
        comboBox.addItemListener(this);

        if (selectedItem != null) {
            comboBox.setSelectedItem(selectedItem);
        }
    }

    /*
     * The item map will contain items for each component or items for each attribute type.
     */
    private void buildItemsMap() {
        final UIDefaults defaults = UIManager.getLookAndFeelDefaults();
        // Build of Map of items, and a Map of attributes for each item
        for (final Object key : new HashSet<>(defaults.keySet())) {
            final Object value = defaults.get(key);
            final String itemName = getItemName(key.toString(), value);
            if (itemName == null) {
                continue;
            }
            // Get the attribute map for this component, or
            // create a map when one is not found
            final TreeMap<String, Object> attributeMap = items.computeIfAbsent(itemName, k -> new TreeMap<>());
            // Add the attribute to the map for this component
            attributeMap.put(key.toString(), value);
        }
    }

    /*
     * Parse the key to determine the item name to use
     */
    private String getItemName(final String key, final Object value) {
        // Seems like this is an old check required for JDK1.4.2
        if (key.startsWith("class") || key.startsWith("javax")) {
            return null;
        }
        if (byComponent.isSelected()) {
            return getComponentName(key, value);
        } else {
            return getValueName(key, value);
        }
    }

    private String getComponentName(final String key, final Object value) {
        // The key is of the form:
        // "componentName.componentProperty", or
        // "componentNameUI", or
        // "someOtherString"
        String componentName;
        final int pos = componentNameEndOffset(key);
        if (pos != -1) {
            componentName = key.substring(0, pos);
        } else if (key.endsWith("UI")) {
            componentName = key.substring(0, key.length() - 2);
        } else if (value instanceof ColorUIResource) {
            componentName = "System Colors";
        } else {
            componentName = "Miscellaneous";
        }
        // Fix inconsistency
        if (componentName.equals("Checkbox")) {
            componentName = "CheckBox";
        }
        return componentName;
    }

    private String getValueName(final String key, final Object value) {
        if (value instanceof Icon) {
            return "Icon";
        } else if (value instanceof Font) {
            return "Font";
        } else if (value instanceof Border) {
            return "Border";
        } else if (value instanceof Color) {
            return "Color";
        } else if (value instanceof Insets) {
            return "Insets";
        } else if (value instanceof Boolean) {
            return "Boolean";
        } else if (value instanceof Dimension) {
            return "Dimension";
        } else if (value instanceof Number) {
            return "Number";
        } else if (value instanceof Painter) {
            return "Painter";
        } else if (key.endsWith("UI")) {
            return "UI";
        } else if (key.endsWith("InputMap")) {
            return "InputMap";
        } else if (key.endsWith("RightToLeft")) {
            return "InputMap";
        } else if (key.endsWith("radient")) {
            return "Gradient";
        } else {
            return "The Rest";
        }
    }

    private int componentNameEndOffset(final String key) {
        // Handle Nimbus properties first
        // "ComboBox.scrollPane", "Table.editor" and "Tree.cellEditor"
        // have different format even within the Nimbus properties.
        // (the component name is specified in quotes)
        if (key.startsWith("\"")) {
            return key.indexOf("\"", 1) + 1;
        }
        int pos = key.indexOf(":");
        if (pos != -1) {
            return pos;
        }
        pos = key.indexOf("[");
        if (pos != -1) {
            return pos;
        }
        // Handle normal properties
        return key.indexOf(".");
    }

    /*
     * Implement the ItemListener interface
     */
    public void itemStateChanged(final ItemEvent e) {
        final String itemName = (String) e.getItem();
        changeTableModel(itemName);
        updateRowHeights();
        selectedItem = itemName;
    }

    /*
     * Change the TableModel in the table for the selected item
     */
    private void changeTableModel(final String itemName) {
        // The model has been created previously so just use it
        DefaultTableModel model = models.get(itemName);
        if (model != null) {
            table.setModel(model);
            return;
        }
        // Create a new model for the requested item
        // and addAtHead the attributes of the item to the model
        model = new DefaultTableModel(COLUMN_NAMES, 0);
        final Map<String, Object> attributes = items.get(itemName);
        for (final Object o : attributes.keySet()) {
            final String attribute = o.toString();
            Object value = attributes.get(attribute);
            final Vector<Object> row = new Vector<>(3);
            row.add(attribute);
            if (value != null) {
                row.add(value.toString());
                if (value instanceof Icon) {
                    value = new SafeIcon((Icon) value);
                } else if (value instanceof String && value.toString().endsWith(".wav")) {
                    value = IconLoader.get(UIManagerDefaults.class).getIcon("icon/sound.svg", 24, 24);
                }
                row.add(value);
            } else {
                row.add("null");
                row.add("");
            }
            model.addRow(row);
        }
        table.setModel(model);
        models.put(itemName, model);
    }

    /*
     * Some rows containing icons, may need to be sized taller to fully display the icon.
     */
    private void updateRowHeights() {
        try {
            for (int row = 0; row < table.getRowCount(); row++) {
                int rowHeight = table.getRowHeight();

                for (int column = 0; column < table.getColumnCount(); column++) {
                    final Component comp = table.prepareRenderer(table.getCellRenderer(row, column), row, column);
                    rowHeight = Math.max(rowHeight, comp.getPreferredSize().height);
                }
                table.setRowHeight(row, rowHeight);
            }
        } catch (final ClassCastException ignored) {
        }
    }
}

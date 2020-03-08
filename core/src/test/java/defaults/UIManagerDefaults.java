package defaults;
/*
 *	This programs uses the information found in the UIManager
 *  to create a table of key/value pairs for each Swing component.
 */

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.*;
import com.github.weisj.darklaf.ui.table.DarkColorTableCellRendererEditor;
import org.jdesktop.swingx.JXTaskPane;
import org.jdesktop.swingx.JXTaskPaneContainer;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.*;

public class UIManagerDefaults implements ItemListener {
    private static final String[] COLUMN_NAMES = {"Key", "Value", "Sample"};
    private static String selectedItem;

    private final JComponent contentPane;
    private final TreeMap<String, TreeMap<String, Object>> items;
    private final HashMap<String, DefaultTableModel> models;
    private JMenuBar menuBar;
    private JComboBox<String> comboBox;
    private JRadioButton byComponent;
    private JTable table;

    /*
     * Constructor
     */
    private UIManagerDefaults() {
        new JXTaskPaneContainer();
        new JXTaskPane();
        items = new TreeMap<>();
        models = new HashMap<>();

        contentPane = new JPanel(new BorderLayout());
        contentPane.add(buildNorthComponent(), BorderLayout.NORTH);
        contentPane.add(buildCenterComponent(), BorderLayout.CENTER);

        resetComponents();
    }

    public static void main(final String[] args) {
        LafManager.install();
        SwingUtilities.invokeLater(UIManagerDefaults::createAndShowGUI);
    }

    /*
     *  Build a GUI using the content pane and menu bar of defaults.UIManagerDefaults
     */
    private static void createAndShowGUI() {
        final UIManagerDefaults application = new UIManagerDefaults();
        final JFrame frame = new JFrame("UIManager Defaults");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setJMenuBar(application.getMenuBar());
        frame.getContentPane().add(application.getContentPane());
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);
    }

    /*
     *  A menu can also be added which provides the ability to switch
     *  between different LAF's.
     */
    private JMenuBar getMenuBar() {
        if (menuBar == null) {
            menuBar = createMenuBar();
        }

        return menuBar;
    }

    /*
     *  The content pane should be added to a high level container
     */

    public JComponent getContentPane() {
        return contentPane;
    }

    /**
     * Create menu bar
     */

    private JMenuBar createMenuBar() {
        final JMenuBar menuBar = new JMenuBar();
        menuBar.add(createFileMenu());
        menuBar.add(createLAFMenu());
        return menuBar;
    }

    /**
     * Create menu items for the Application menu
     */

    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("Application");
        menu.setMnemonic('A');
        ExitAction item = new ExitAction();
        menu.add(item);
        return menu;
    }

    /**
     * Create menu items for the Look & Feel menu
     */

    private JMenu createLAFMenu() {
        final ButtonGroup bg = new ButtonGroup();
        final JMenu menu = new JMenu("Look & Feel");
        menu.setMnemonic('L');
        final String lafId = LafManager.getTheme().getName();

        Theme[] themes = {new DarculaTheme(), new IntelliJTheme(), new SolarizedDarkTheme(), new SolarizedLightTheme()};
        for (Theme theme : themes) {
            final String name = theme.getName();
            final Action action = new AbstractAction(name) {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    LafManager.install(theme);
                }
            };
            final JRadioButtonMenuItem mi = new JRadioButtonMenuItem(action);
            menu.add(mi);
            bg.add(mi);
            if (name.equals(lafId)) {
                mi.setSelected(true);
            }
        }
        return menu;
    }

    /*
     *  This panel is added to the North of the content pane
     */

    private JComponent buildNorthComponent() {
        comboBox = new JComboBox<String>() {
            @Override
            public Dimension getPreferredSize() {
                return new Dimension(200, getUI().getPreferredSize(this).height);
            }
        };

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

    /*
     *  This panel is added to the Center of the content pane
     */


    private JComponent buildCenterComponent() {
        final DefaultTableModel model = new DefaultTableModel(COLUMN_NAMES, 0);
        table = new JTable(model);
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

        return new JScrollPane(table);
    }

    /*
     *  When the LAF is changed we need to reset the content pane
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
     *	The item map will contain items for each component or
     *  items for each attribute type.
     */
    private void buildItemsMap() {
        final UIDefaults defaults = UIManager.getLookAndFeelDefaults();
        //  Build of Map of items and a Map of attributes for each item
        for (final Object key : new HashSet<>(defaults.keySet())) {
            final Object value = defaults.get(key);
            final String itemName = getItemName(key.toString(), value);
            if (itemName == null) {
                continue;
            }
            //  Get the attribute map for this componenent, or
            //  create a map when one is not found
            final TreeMap<String, Object> attributeMap =
                    items.computeIfAbsent(itemName, k -> new TreeMap<>());
            //  Add the attribute to the map for this componenent
            attributeMap.put(key.toString(), value);
        }
    }

    /*
     *  Parse the key to determine the item name to use
     */

    private String getItemName(final String key, final Object value) {
        //  Seems like this is an old check required for JDK1.4.2
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
        //  The key is of the form:
        //  "componentName.componentProperty", or
        //  "componentNameUI", or
        //  "someOtherString"
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
        //  Fix inconsistency
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
        //  Handle Nimbus properties first
        //  "ComboBox.scrollPane", "Table.editor" and "Tree.cellEditor"
        //  have different format even within the Nimbus properties.
        //  (the component name is specified in quotes)
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
        //  Handle normal properties
        return key.indexOf(".");
    }

    /*
     *  Implement the ItemListener interface
     */
    public void itemStateChanged(final ItemEvent e) {
        final String itemName = (String) e.getItem();
        changeTableModel(itemName);
        updateRowHeights();
        selectedItem = itemName;
    }

    /*
     *  Change the TableModel in the table for the selected item
     */
    private void changeTableModel(final String itemName) {
        //  The model has been created previously so just use it
        DefaultTableModel model = models.get(itemName);
        if (model != null) {
            table.setModel(model);
            return;
        }
        //  Create a new model for the requested item
        //  and addAtHead the attributes of the item to the model
        model = new DefaultTableModel(COLUMN_NAMES, 0);
        final Map<String, Object> attributes = items.get(itemName);
        for (final Object o : attributes.keySet()) {
            final String attribute = (String) o;
            Object value = attributes.get(attribute);
            final Vector<Object> row = new Vector<>(3);
            row.add(attribute);
            if (value != null) {
                row.add(value instanceof Boolean ? value : value.toString());
                if (value instanceof Icon) {
                    value = new SafeIcon((Icon) value);
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
     *  Some rows containing icons, may need to be sized taller to fully
     *  display the icon.
     */
    private void updateRowHeights() {
        try {
            for (int row = 0; row < table.getRowCount(); row++) {
                int rowHeight = table.getRowHeight();

                for (int column = 0; column < table.getColumnCount(); column++) {
                    final Component comp =
                            table.prepareRenderer(table.getCellRenderer(row, column), row, column);
                    rowHeight = Math.max(rowHeight, comp.getPreferredSize().height);
                }
                table.setRowHeight(row, rowHeight);
            }
        } catch (final ClassCastException ignored) {
        }
    }

}

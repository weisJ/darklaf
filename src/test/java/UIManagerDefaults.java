/*
 *	This programs uses the information found in the UIManager
 *  to create a table of key/value pairs for each Swing component.
 */

import com.weis.darklaf.DarkLafInfo;
import com.weis.darklaf.LafManager;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

public class UIManagerDefaults implements ActionListener, ItemListener {
    private static final String[] COLUMN_NAMES = {"Key", "Value", "Sample"};
    @Nullable
    private static String selectedItem;

    @NotNull
    private final JComponent contentPane;
    @NotNull
    private final TreeMap<String, TreeMap<String, Object>> items;
    @NotNull
    private final HashMap<String, DefaultTableModel> models;
    private JMenuBar menuBar;
    private JComboBox<String> comboBox;
    private JRadioButton byComponent;
    private JTable table;

    /*
     * Constructor
     */
    public UIManagerDefaults() {
        items = new TreeMap<>();
        models = new HashMap<>();

        contentPane = new JPanel(new BorderLayout());
        contentPane.add(buildNorthComponent(), BorderLayout.NORTH);
        contentPane.add(buildCenterComponent(), BorderLayout.CENTER);

        resetComponents();
    }

    /*
     *  Build a GUI using the content pane and menu bar of UIManagerDefaults
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
     *  UIManagerDefaults Main. Called only if we're an application.
     */
    public static void main(final String[] args) {
        LafManager.loadLaf(LafManager.Theme.Dark);
        SwingUtilities.invokeLater(UIManagerDefaults::createAndShowGUI);
    }

    /*
     *  The content pane should be added to a high level container
     */
    @NotNull
    public JComponent getContentPane() {
        return contentPane;
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
     *  This panel is added to the North of the content pane
     */
    @NotNull
    private JComponent buildNorthComponent() {
        comboBox = new JComboBox<>();

        final JLabel label = new JLabel("Select Item:");
        label.setDisplayedMnemonic('S');
        label.setLabelFor(comboBox);

        byComponent = new JRadioButton("By Component", true);
        byComponent.setMnemonic('C');
        byComponent.addActionListener(this);

        final JRadioButton byValueType = new JRadioButton("By Value Type");
        byValueType.setMnemonic('V');
        byValueType.addActionListener(this);

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
    @NotNull
    private JComponent buildCenterComponent() {
        final DefaultTableModel model = new DefaultTableModel(COLUMN_NAMES, 0);
        table = new JTable(model);
        table.setAutoCreateColumnsFromModel(false);
        table.getColumnModel().getColumn(0).setPreferredWidth(250);
        table.getColumnModel().getColumn(1).setPreferredWidth(500);
        table.getColumnModel().getColumn(2).setPreferredWidth(100);
        table.getColumnModel().getColumn(2).setCellRenderer(new SampleRenderer());
        final Dimension d = table.getPreferredSize();
        d.height = 350;
        table.setPreferredScrollableViewportSize(d);

        return new JScrollPane(table);
    }

    /*
     *  When the LAF is changed we need to reset the content pane
     */
    private void resetComponents() {
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
        comboBox.requestFocusInWindow();

        if (selectedItem != null) {
            comboBox.setSelectedItem(selectedItem);
        }
    }

    /*
     *	The item map will contain items for each component or
     *  items for each attribute type.
     */
    @NotNull
    private TreeMap buildItemsMap() {
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
        return items;
    }

    /*
     *  Parse the key to determine the item name to use
     */
    @Nullable
    private String getItemName(@NotNull final String key, final Object value) {
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

    private String getComponentName(@NotNull final String key, final Object value) {
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

    private int componentNameEndOffset(@NotNull final String key) {
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

    @NotNull
    private String getValueName(@NotNull final String key, final Object value) {
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

    /**
     * Create menu bar
     */
    @NotNull
    private JMenuBar createMenuBar() {
        final JMenuBar menuBar = new JMenuBar();
        menuBar.add(createFileMenu());
        menuBar.add(createLAFMenu());
        return menuBar;
    }

    /**
     * Create menu items for the Application menu
     */
    @NotNull
    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("Application");
        menu.setMnemonic('A');
        var item = new ExitAction();
        menu.add(item);
        return menu;
    }

    /**
     * Create menu items for the Look & Feel menu
     */
    @NotNull
    private JMenu createLAFMenu() {
        final ButtonGroup bg = new ButtonGroup();
        final JMenu menu = new JMenu("Look & Feel");
        menu.setMnemonic('L');
        final String lafId = UIManager.getLookAndFeel().getID();
        final UIManager.LookAndFeelInfo[] lafInfo = UIManager.getInstalledLookAndFeels();

        for (final UIManager.LookAndFeelInfo lookAndFeelInfo : lafInfo) {
            final String laf = lookAndFeelInfo.getClassName();
            final String name = lookAndFeelInfo.getName();
            final Action action = new ChangeLookAndFeelAction(this, laf, name);
            final JRadioButtonMenuItem mi = new JRadioButtonMenuItem(action);
            menu.add(mi);
            bg.add(mi);
            if (name.equals(lafId)) {
                mi.setSelected(true);
            }
        }
        var info = new DarkLafInfo();
        Action action = new ChangeLookAndFeelAction(this, info.getClassName(), info.getName());
        var mi = new JRadioButtonMenuItem(action);
        menu.add(mi);
        bg.add(mi);
        mi.setSelected(true);
        return menu;
    }

    /*
     *  Implement the ActionListener interface
     */
    public void actionPerformed(final ActionEvent e) {
        selectedItem = null;
        resetComponents();
        comboBox.requestFocusInWindow();
    }

    /*
     *  Implement the ItemListener interface
     */
    public void itemStateChanged(@NotNull final ItemEvent e) {
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
                row.add(value.toString());
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
        } catch (@NotNull final ClassCastException ignored) {
        }
    }

    /**
     * Thanks to Jeanette for the use of this code found at:
     *
     * <p>https://jdnc-incubator.dev.java.net/source/browse/jdnc-incubator/src/kleopatra/java/org
     * /jdesktop/swingx/renderer/UIPropertiesViewer.java?rev=1.2&view=markup
     *
     * <p>Some ui-icons misbehave in that they unconditionally class-cast to the component type they
     * are mostly painted on. Consequently they blow up if we are trying to paint them anywhere else
     * (f.i. in a renderer).
     *
     * <p>This Icon is an adaption of a cool trick by Darryl Burke found at
     * http://tips4java.wordpress.com/2008/12/18/icon-table-cell-renderer
     *
     * <p>The base idea is to instantiate a component of the type expected by the icon, let it paint
     * into the graphics of a bufferedImage and create an ImageIcon from it. In subsequent calls the
     * ImageIcon is used.
     */
    private static final class SafeIcon implements Icon {
        private final Icon wrappee;
        private Icon standIn;

        @Contract(pure = true)
        private SafeIcon(final Icon wrappee) {
            this.wrappee = wrappee;
        }

        @Override
        public void paintIcon(final Component c, @NotNull final Graphics g, final int x, final int y) {
            if (standIn == this) {
                paintFallback(c, g, x, y);
            } else if (standIn != null) {
                standIn.paintIcon(c, g, x, y);
            } else {
                try {
                    wrappee.paintIcon(c, g, x, y);
                } catch (@NotNull final ClassCastException e) {
                    createStandIn(e, x, y);
                    standIn.paintIcon(c, g, x, y);
                }
            }
        }

        @Override
        public int getIconWidth() {
            return wrappee.getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return wrappee.getIconHeight();
        }

        private void createStandIn(@NotNull final ClassCastException e, final int x, final int y) {
            try {
                final Class<?> clazz = getClass(e);
                final JComponent standInComponent = getSubstitute(clazz);
                standIn = createImageIcon(standInComponent, x, y);
            } catch (@NotNull final Exception e1) {
                // something went wrong - fallback to this painting
                standIn = this;
            }
        }

        @Contract("_, _, _ -> new")
        @NotNull
        private Icon createImageIcon(final JComponent standInComponent, final int x, final int y) {
            final BufferedImage image =
                    new BufferedImage(getIconWidth(), getIconHeight(), BufferedImage.TYPE_INT_ARGB);
            final Graphics g = image.createGraphics();
            try {
                wrappee.paintIcon(standInComponent, g, 0, 0);
                return new ImageIcon(image);
            } finally {
                g.dispose();
            }
        }

        private JComponent getSubstitute(@NotNull final Class<?> clazz) throws IllegalAccessException {
            JComponent standInComponent = null;
            try {
                standInComponent = (JComponent) clazz.getDeclaredConstructor().newInstance();
            } catch (@NotNull final InstantiationException e) {
                standInComponent = new AbstractButton() {
                };
                ((AbstractButton) standInComponent).setModel(new DefaultButtonModel());
            } catch (NoSuchMethodException | InvocationTargetException e) {
                e.printStackTrace();
            }
            return standInComponent;
        }

        private Class<?> getClass(@NotNull final ClassCastException e) throws ClassNotFoundException {
            String className = e.getMessage();
            className = className.substring(className.lastIndexOf(" ") + 1);
            return Class.forName(className);
        }

        private void paintFallback(
                final Component c, @NotNull final Graphics g, final int x, final int y) {
            g.drawRect(x, y, getIconWidth(), getIconHeight());
            g.drawLine(x, y, x + getIconWidth(), y + getIconHeight());
            g.drawLine(x + getIconWidth(), y, x, y + getIconHeight());
        }
    }

    /*
     *  Render the value based on its class.
     */
    private static final class SampleRenderer extends JLabel implements TableCellRenderer {
        private SampleRenderer() {
            super();
            setHorizontalAlignment(SwingConstants.CENTER);
            setOpaque(true);
        }

        @Contract("_, _, _, _, _, _ -> this")
        @NotNull
        public Component getTableCellRendererComponent(
                final JTable table,
                final Object sample,
                final boolean isSelected,
                final boolean hasFocus,
                final int row,
                final int column) {
            setBackground(null);
            setBorder(null);
            setIcon(null);
            setText("");

            if (sample instanceof Color) {
                setBackground((Color) sample);
            } else if (sample instanceof Border) {
                setBorder((Border) sample);
            } else if (sample instanceof Font) {
                setText("Sample");
                setFont((Font) sample);
            } else if (sample instanceof Icon) {
                setIcon((Icon) sample);
            }
            return this;
        }

        /*
         *  Some icons are painted using inner classes and are not meant to be
         *  shared by other items. This code will catch the
         *  ClassCastException that is thrown.
         */
        public void paint(final Graphics g) {
            try {
                super.paint(g);
            } catch (@NotNull final Exception e) {
                e.printStackTrace();
            }
        }
    }

    /*
     *  Change the LAF and recreate the UIManagerDefaults so that the properties
     *  of the new LAF are correctly displayed.
     */
    private static final class ChangeLookAndFeelAction extends AbstractAction {
        private final UIManagerDefaults defaults;
        private final String laf;

        private ChangeLookAndFeelAction(
                final UIManagerDefaults defaults, final String laf, final String name) {
            this.defaults = defaults;
            this.laf = laf;
            putValue(Action.NAME, name);
            putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
        }

        public void actionPerformed(@NotNull final ActionEvent e) {
            try {
                UIManager.setLookAndFeel(laf);
                defaults.resetComponents();

                final JMenuItem mi = (JMenuItem) e.getSource();
                final JPopupMenu popup = (JPopupMenu) mi.getParent();
                final JRootPane rootPane = SwingUtilities.getRootPane(popup.getInvoker());
                SwingUtilities.updateComponentTreeUI(rootPane);
                //  Use custom decorations when supported by the LAF
                final JFrame frame = (JFrame) SwingUtilities.windowForComponent(rootPane);
                frame.dispose();

                if (UIManager.getLookAndFeel().getSupportsWindowDecorations()) {
                    frame.setUndecorated(true);
                    frame.getRootPane().setWindowDecorationStyle(JRootPane.FRAME);
                } else {
                    frame.setUndecorated(false);
                }
                frame.setVisible(true);
            } catch (@NotNull final Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    /*
     *	Close the frame
     */
    private static final class ExitAction extends AbstractAction {
        private ExitAction() {
            putValue(Action.NAME, "Exit");
            putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
            putValue(Action.MNEMONIC_KEY, KeyEvent.VK_X);
        }

        public void actionPerformed(final ActionEvent e) {
            System.exit(0);
        }
    }
}

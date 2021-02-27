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
package com.github.weisj.darklaf.ui.filechooser;

import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.function.Supplier;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.event.AncestorEvent;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.listener.AncestorAdapter;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TextTableCellEditorBorder;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellEditor;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellEditorDelegate;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class DarkFilePane extends DarkFilePaneUIBridge {

    protected TableCellEditor tableCellEditor;

    public DarkFilePane(final FileChooserUIAccessor fileChooserUIAccessor) {
        super(fileChooserUIAccessor);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        kiloByteString = UIManager.getString("FileChooser.fileSizeKiloBytes");
        megaByteString = UIManager.getString("FileChooser.fileSizeMegaBytes");
        gigaByteString = UIManager.getString("FileChooser.fileSizeGigaBytes");
        editCell = new JTextField();
        editCell.setName("FileChooser.listEditCell");
        editCell.addActionListener(new EditActionListener());
        editCell.setBorder(new TextTableCellEditorBorder());
        editCell.putClientProperty("JTextField.listCellEditor", true);
        editCell.putClientProperty(DarkTextUI.KEY_IS_LIST_EDITOR, true);
    }

    public JPanel createList() {
        JPanel p = new JPanel(new BorderLayout());
        final JFileChooser fileChooser = getFileChooser();

        @SuppressWarnings("serial") // anonymous class
        final JList<Object> list = new JList<Object>() {
            public int getNextMatch(final String prefix, final int startIndex, final Position.Bias bias) {
                ListModel<?> model = getModel();
                int max = model.getSize();
                if (prefix == null || startIndex < 0 || startIndex >= max) {
                    throw new IllegalArgumentException();
                }
                // start search from the next element before/after the selected element
                boolean backwards = (bias == Position.Bias.Backward);
                for (int i = startIndex; backwards ? i >= 0 : i < max; i += (backwards ? -1 : 1)) {
                    String filename = fileChooser.getName((File) model.getElementAt(i));
                    if (filename.regionMatches(true, 0, prefix, 0, prefix.length())) {
                        return i;
                    }
                }
                return -1;
            }
        };
        list.setCellRenderer(new DarkFileRenderer());
        list.setLayoutOrientation(JList.VERTICAL_WRAP);
        LookAndFeel.installColors(list, "FileView.background", "FileView.foreground");

        // 4835633 : tell BasicListUI that this is a file list
        list.putClientProperty("List.isFileList", Boolean.TRUE);
        list.putClientProperty("JList.fullRowSelection", fullRowSelection);

        if (listViewWindowsStyle) {
            list.addFocusListener(repaintListener);
        }

        updateListRowCount(list);

        getModel().addListDataListener(new ListDataListener() {
            public void intervalAdded(final ListDataEvent e) {
                updateListRowCount(list);
            }

            public void intervalRemoved(final ListDataEvent e) {
                updateListRowCount(list);
            }

            public void contentsChanged(final ListDataEvent e) {
                if (isShowing()) {
                    clearSelection();
                }
                updateListRowCount(list);
            }
        });

        getModel().addPropertyChangeListener(this);

        if (fileChooser.isMultiSelectionEnabled()) {
            list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        } else {
            list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        }
        list.setModel(new SortableListModel());

        list.addListSelectionListener(createListSelectionListener());
        list.addMouseListener(getMouseHandler());

        OverlayScrollPane overlayScrollPane = new OverlayScrollPane(list);
        JScrollPane scrollPane = overlayScrollPane.getScrollPane();
        if (listViewBackground != null) {
            list.setBackground(listViewBackground);
            p.setBackground(listViewBackground);
        }
        if (listViewBorder != null) {
            scrollPane.setBorder(listViewBorder);
        }

        list.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, filesListAccessibleName);

        p.add(overlayScrollPane, BorderLayout.CENTER);
        return p;
    }

    @Override
    public JPanel createDetailsView() {
        final JFileChooser chooser = getFileChooser();

        JPanel p = new JPanel(new BorderLayout());

        @SuppressWarnings("serial") // anonymous class
        final JTable detailsTable = new JTable(getDetailsTableModel()) {
            public void tableChanged(final TableModelEvent e) {
                super.tableChanged(e);

                if (e.getFirstRow() == TableModelEvent.HEADER_ROW) {
                    // update header with possibly changed column set
                    updateDetailsColumnModel(this);
                }
            }

            // Handle Escape key events here
            protected boolean processKeyBinding(final KeyStroke ks, final KeyEvent e, final int condition,
                    final boolean pressed) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE && getCellEditor() == null) {
                    // We are not editing, forward to filechooser.
                    chooser.dispatchEvent(e);
                    return true;
                }
                return super.processKeyBinding(ks, e, condition, pressed);
            }
        };
        int rowHeight = UIManager.getInt("FileChooser.rowHeight");
        if (rowHeight > 0) {
            detailsTable.setRowHeight(rowHeight);
        }
        detailsTable.setRowSorter(getRowSorter());
        detailsTable.setAutoCreateColumnsFromModel(false);
        detailsTable.setComponentOrientation(chooser.getComponentOrientation());
        detailsTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        detailsTable.setRowSelectionAllowed(true);
        detailsTable.setShowGrid(false);
        detailsTable.putClientProperty("JTable.autoStartsEdit", Boolean.FALSE);
        detailsTable.addKeyListener(detailsKeyListener);
        detailsTable.putClientProperty(DarkTableUI.KEY_FULL_ROW_FOCUS_BORDER, true);
        detailsTable.putClientProperty(DarkTableUI.KEY_FILE_CHOOSER_PARENT,
                (Supplier<JFileChooser>) this::getFileChooser);
        detailsTable.putClientProperty("JTable.fileNameColumnIndex", COLUMN_FILENAME);

        Font font = list.getFont();
        detailsTable.setFont(font);

        TableCellRenderer headerRenderer =
                new AlignableTableHeaderRenderer(detailsTable.getTableHeader().getDefaultRenderer());
        detailsTable.getTableHeader().setDefaultRenderer(headerRenderer);
        TableCellRenderer cellRenderer = new DetailsTableCellRenderer(chooser);
        detailsTable.setDefaultRenderer(Object.class, cellRenderer);

        if (getFileChooser().isMultiSelectionEnabled()) {
            detailsTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        } else {
            detailsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        }

        detailsTable.addMouseListener(getMouseHandler());

        // 4835633 : tell BasicTableUI that this is a file list
        detailsTable.putClientProperty(DarkTableUI.KEY_IS_FILE_LIST, Boolean.TRUE);

        if (listViewWindowsStyle) {
            detailsTable.addFocusListener(repaintListener);
        }

        // TAB/SHIFT-TAB should transfer focus and ENTER should select an item.
        // We don't want them to navigate within the table
        ActionMap am = SwingUtilities.getUIActionMap(detailsTable);
        am.remove("selectNextRowCell");
        am.remove("selectPreviousRowCell");
        am.remove("selectNextColumnCell");
        am.remove("selectPreviousColumnCell");
        detailsTable.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, null);
        detailsTable.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, null);

        OverlayScrollPane overlayScrollPane = new OverlayScrollPane(detailsTable);
        JScrollPane scrollPane = overlayScrollPane.getScrollPane();
        scrollPane.setComponentOrientation(chooser.getComponentOrientation());
        LookAndFeel.installColors(scrollPane.getViewport(), "FileView.background", "FileView.foreground");
        LookAndFeel.installColors(detailsTable, "FileView.background", "FileView.foreground");

        // Adjust width of first column so the table fills the viewport when
        // first displayed (temporary listener).
        scrollPane.addComponentListener(new ComponentAdapter() {
            public void componentResized(final ComponentEvent e) {
                JScrollPane sp = (JScrollPane) e.getComponent();
                fixNameColumnWidth(sp.getViewport().getSize().width);
                sp.removeComponentListener(this);
            }
        });

        detailsTable.setForeground(list.getForeground());
        detailsTable.setBackground(list.getBackground());

        if (listViewBorder != null) {
            scrollPane.setBorder(listViewBorder);
        }
        p.add(overlayScrollPane, BorderLayout.CENTER);

        detailsTableModel.fireTableStructureChanged();

        detailsTable.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, filesDetailsAccessibleName);

        return p;
    }

    protected void cancelEdit() {
        if (editFile != null) {
            editFile = null;
            list.remove(editCell);
            list.putClientProperty("JList.isEditing", false);
            repaint();
        } else if (detailsTable != null && detailsTable.isEditing()) {
            detailsTable.getCellEditor().cancelCellEditing();
        }
    }

    protected void editFileName(final int index) {
        JFileChooser chooser = getFileChooser();
        File currentDirectory = chooser.getCurrentDirectory();

        if (readOnly || !canWrite(currentDirectory, chooser)) {
            return;
        }

        ensureIndexIsVisible(index);
        switch (viewType) {
            case VIEWTYPE_LIST:
                editFile = (File) getModel().getElementAt(getRowSorter().convertRowIndexToModel(index));
                Rectangle r = list.getCellBounds(index, index);
                list.add(editCell);
                editCell.setText(chooser.getName(editFile));
                ComponentOrientation orientation = list.getComponentOrientation();
                editCell.setComponentOrientation(orientation);

                Icon icon = chooser.getIcon(editFile);

                // PENDING - grab padding (4) below from defaults table.
                int editX = icon == null ? 20 : icon.getIconWidth() + 4;

                int gap = 0;
                ListCellRenderer<?> renderer = list.getCellRenderer();
                if (renderer instanceof JLabel) {
                    gap = ((JLabel) renderer).getIconTextGap() - 1;
                }
                if (orientation.isLeftToRight()) {
                    editCell.setBounds(editX + r.x + gap, r.y, r.width - editX - gap, r.height);
                } else {
                    editCell.setBounds(r.x, r.y, r.width - editX - gap, r.height);
                }
                list.putClientProperty("JList.isEditing", true);
                editCell.requestFocusInWindow();
                editCell.selectAll();
                break;

            case VIEWTYPE_DETAILS:
                detailsTable.editCellAt(index, COLUMN_FILENAME);
                break;
        }
    }

    public JPopupMenu getComponentPopupMenu() {
        JPopupMenu popupMenu = getFileChooser().getComponentPopupMenu();
        if (popupMenu != null) {
            return popupMenu;
        }

        JMenu viewMenu = getViewMenu();
        if (contextMenu == null) {
            contextMenu = new JPopupMenu();
            if (viewMenu != null) {
                contextMenu.add(viewMenu);
                if (listViewWindowsStyle) {
                    contextMenu.addSeparator();
                }
            }
            ActionMap actionMap = getActionMap();
            Action refreshAction = actionMap.get(ACTION_REFRESH);
            Action newFolderAction = actionMap.get(ACTION_NEW_FOLDER);
            Action renameAction = actionMap.get(ACTION_EDIT_FILE_NAME);
            if (refreshAction != null) {
                contextMenu.add(refreshAction).setIcon(UIManager.getIcon("FileChooser.refreshIcon"));
            }
            if (renameAction != null) {
                JMenuItem menuItem = new JMenuItem(renameAction);
                menuItem.setText("Rename");
                contextMenu.add(menuItem);
            }
            if (newFolderAction != null) {
                contextMenu.add(newFolderAction).setIcon(UIManager.getIcon("FileChooser.newFolderIcon"));
            }
        }
        if (viewMenu != null) {
            viewMenu.getPopupMenu().setInvoker(viewMenu);
        }
        return contextMenu;
    }

    @Override
    public JMenu getViewMenu() {
        if (viewMenu == null) {
            viewMenu = new JMenu(viewMenuLabelText);
            ButtonGroup viewButtonGroup = new ButtonGroup();

            for (int i = 0; i < VIEWTYPE_COUNT; i++) {
                JRadioButtonMenuItem mi = new JRadioButtonMenuItem(new ViewTypeAction(i));
                switch (i) {
                    case VIEWTYPE_DETAILS:
                        mi.setIcon(UIManager.getIcon("FileChooser.detailsViewIcon"));
                        break;
                    case VIEWTYPE_LIST:
                        mi.setIcon(UIManager.getIcon("FileChooser.listViewIcon"));
                        break;
                    default:
                        break;
                }
                viewButtonGroup.add(mi);
                viewMenu.add(mi);
            }
            updateViewMenu();
        }
        return viewMenu;
    }

    @Override
    protected void updateViewMenu() {
        if (viewMenu != null) {
            Component[] comps = viewMenu.getMenuComponents();
            for (Component comp : comps) {
                if (comp instanceof JRadioButtonMenuItem) {
                    JRadioButtonMenuItem mi = (JRadioButtonMenuItem) comp;
                    if (((ViewTypeAction) mi.getAction()).viewType == viewType) {
                        mi.setSelected(true);
                    }
                }
            }
        }
    }

    @Override
    protected Handler getMouseHandler() {
        if (handler == null) {
            handler = new DarkHandler();
        }
        return handler;
    }

    protected class DarkHandler extends Handler {

        @Override
        public void mouseClicked(MouseEvent evt) {
            JComponent source = (JComponent) evt.getSource();

            int index;
            if (source instanceof JList) {
                index = list.locationToIndex(evt.getPoint());
            } else if (source instanceof JTable) {
                JTable table = (JTable) source;
                Point p = evt.getPoint();
                index = table.rowAtPoint(p);

                boolean pointOutsidePrefSize =
                        SwingUtilities2.pointOutsidePrefSize(table, index, table.columnAtPoint(p), p);

                if (pointOutsidePrefSize && !fullRowSelection) {
                    return;
                }

                // Translate point from table to list
                if (index >= 0 && list != null && listSelectionModel.isSelectedIndex(index)) {

                    // Make a new event with the list as source, placing the
                    // click in the corresponding list cell.
                    Rectangle r = list.getCellBounds(index, index);
                    MouseEvent newEvent = new MouseEvent(list, evt.getID(), evt.getWhen(), evt.getModifiersEx(),
                            r.x + 1, r.y + r.height / 2, evt.getXOnScreen(), evt.getYOnScreen(), evt.getClickCount(),
                            evt.isPopupTrigger(), evt.getButton());
                    SwingUtilities.convertMouseEvent(list, newEvent, list);
                    evt = newEvent;
                }
            } else {
                return;
            }

            if (index >= 0 && SwingUtilities.isLeftMouseButton(evt)) {
                JFileChooser fc = getFileChooser();

                // For single click, we handle editing file name
                if (evt.getClickCount() == 1 && source instanceof JList) {
                    if ((!fc.isMultiSelectionEnabled() || fc.getSelectedFiles().length <= 1)
                            && listSelectionModel.isSelectedIndex(index) && getEditIndex() == index && editFile == null
                            && DarkUIUtil.isOverText(evt, index, list)) {
                        editFileName(index);
                    } else {
                        setEditIndex(index);
                    }
                } else if (evt.getClickCount() == 2) {
                    // on double click (open or drill down one directory) be
                    // sure to clear the edit index
                    resetEditIndex();
                }
            }

            // Forward event to Basic
            if (getDoubleClickListener() != null) {
                list.putClientProperty("List.isFileList", false);
                getDoubleClickListener().mouseClicked(evt);
                list.putClientProperty("List.isFileList", true);
            }
        }
    }

    public class DarkFileRenderer extends DefaultListCellRenderer {

        @Override
        public Component getListCellRendererComponent(final JList<?> list, final Object value, final int index,
                final boolean isSelected, final boolean cellHasFocus) {
            Component comp = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            if (comp instanceof JLabel) {
                File file = (File) value;
                String fileName = getFileChooser().getName(file);
                ((JLabel) comp).setText(fileName);
                Icon icon = getFileChooser().getIcon(file);
                if (icon != null) {
                    ((JLabel) comp).setIcon(icon);
                } else {
                    if (getFileChooser().getFileSystemView().isTraversable(file)) {
                        ((JLabel) comp).setText(fileName + File.separator);
                    }
                }
            }
            return comp;
        }
    }

    protected TableCellEditor getDetailsTableCellEditor() {
        if (tableCellEditor == null) {
            tableCellEditor = new DarkTableCellEditorDelegate(new DetailsTableCellEditor());
        }
        return tableCellEditor;
    }

    protected class DetailsTableCellEditor extends DarkTableCellEditor {

        public DetailsTableCellEditor() {
            editorComponent.addFocusListener(editorFocusListener);
            editorComponent.addAncestorListener(new AncestorAdapter() {
                @Override
                public void ancestorAdded(final AncestorEvent event) {
                    SwingUtilities.invokeLater(() -> {
                        editorComponent.requestFocusInWindow();
                        ((JTextComponent) editorComponent).selectAll();
                    });
                }
            });
        }

        public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                final int row, final int column) {
            Object realValue = value instanceof File ? getFileChooser().getName((File) value) : value;
            return super.getTableCellEditorComponent(table, realValue, isSelected, row, column);
        }
    }
}

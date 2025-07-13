/*
 * MIT License
 *
 * Copyright (c) 2019-2025 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.filechooser;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.LayoutManager;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.io.File;
import java.nio.file.Files;
import java.util.function.Supplier;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalFileChooserUI;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.listener.AncestorAdapter;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.TextTableCellEditorBorder;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellEditor;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellEditorDelegate;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellRenderer;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.AlignmentExt;

public class DarkFileChooserUI extends MetalFileChooserUI {

    private static final int COLUMN_FILENAME = 0;
    private final DarkFileView fileView = createFileView();

    private Icon textFileIcon;
    private Icon imageFileIcon;

    private final AncestorListener editorAncestorListener = new AncestorAdapter() {
        @Override
        public void ancestorAdded(final AncestorEvent event) {
            SwingUtilities.invokeLater(() -> {
                JComponent component = event.getComponent();
                component.requestFocusInWindow();
                if (component instanceof JTextComponent) ((JTextComponent) component).selectAll();
            });
        }
    };

    public DarkFileChooserUI(final JFileChooser fileChooser) {
        super(fileChooser);
    }

    public static ComponentUI createUI(final JComponent c) {
        return new DarkFileChooserUI((JFileChooser) c);
    }

    @Override
    protected void installIcons(final JFileChooser fc) {
        super.installIcons(fc);
        textFileIcon = UIManager.getIcon("FileView.textFileIcon");
        imageFileIcon = UIManager.getIcon("FileView.imageFileIcon");
    }

    @Override
    public void installComponents(final JFileChooser fc) {
        super.installComponents(fc);
        adjustComponents(fc);
    }

    private void adjustComponents(final JFileChooser fc) {
        JPanel topPanel = getComponentWithLayout(fc, 0, JPanel.class, BorderLayout.class);
        if (topPanel != null) {
            JComponent buttonPanel = getComponentWithLayout(topPanel, 0, JComponent.class, BoxLayout.class);
            if (buttonPanel != null) {
                Component[] comps = buttonPanel.getComponents();
                int firstButtonIndex = -1;
                int secondButtonIndex = -1;
                for (int i = comps.length - 1; i >= 0; i--) {
                    Component c = comps[i];
                    if (c instanceof AbstractButton b) {
                        b.setMargin(null);
                        b.putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
                        b.putClientProperty(DarkButtonUI.KEY_SQUARE, true);
                        b.putClientProperty(DarkButtonUI.KEY_THIN, true);
                        b.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
                        if (secondButtonIndex < 0 && firstButtonIndex >= 0) {
                            secondButtonIndex = i;
                        }
                        if (firstButtonIndex < 0) {
                            firstButtonIndex = i;
                        }
                    } else if (c instanceof Box.Filler && secondButtonIndex < 0) {
                        buttonPanel.remove(i);
                    }
                }
                if (firstButtonIndex >= 0 && secondButtonIndex >= 0) {
                    AbstractButton detailsViewButton = (AbstractButton) comps[firstButtonIndex];
                    AbstractButton listViewButton = (AbstractButton) comps[secondButtonIndex];
                    detailsViewButton.setSelectedIcon(UIManager.getIcon("FileChooser.detailsViewSelectedIcon"));
                    detailsViewButton.putClientProperty(DarkButtonUI.KEY_CORNER, AlignmentExt.RIGHT);
                    detailsViewButton.putClientProperty(DarkButtonUI.KEY_LEFT_NEIGHBOUR, listViewButton);

                    listViewButton.setSelectedIcon(UIManager.getIcon("FileChooser.listViewSelectedIcon"));
                    listViewButton.putClientProperty(DarkButtonUI.KEY_CORNER, AlignmentExt.LEFT);
                    listViewButton.putClientProperty(DarkButtonUI.KEY_RIGHT_NEIGHBOUR, detailsViewButton);
                }
            }

            JComboBox<?> directoryComboBox = getComponentWithLayout(topPanel, 2, JComboBox.class, null);
            if (directoryComboBox != null) {
                int maximumRowCount = UIManager.getInt("ComboBox.maximumRowCount");
                if (maximumRowCount > 0)
                    directoryComboBox.setMaximumRowCount(maximumRowCount);
            }
        }
        for (Component comp : fc.getComponents()) {
            if (comp.isPreferredSizeSet()) {
                comp.setPreferredSize(null);
            }
        }
    }

    @Override
    protected FilterComboBoxRenderer createFilterComboBoxRenderer() {
        ListCellRenderer<Object> realRenderer = new DarkFilterComboBoxRenderer();
        return new FilterComboBoxRenderer() {
            @Override
            public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                    boolean cellHasFocus) {
                return realRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            }
        };
    }

    @Override
    protected JPanel createDetailsView(final JFileChooser fc) {
        JPanel p = super.createDetailsView(fc);
        p = wrapInOverlayScrollPane(p);
        OverlayScrollPane sp = getComponentWithLayout(p, 0, OverlayScrollPane.class, null);
        if (sp != null) {
            Component cTable = sp.getScrollPane().getViewport().getView();
            if (cTable instanceof JTable) {
                patchDetailsView((JTable) cTable);
            }
        }
        return p;
    }

    private void patchDetailsView(final JTable table) {
        TableCellRenderer defaultRenderer = table.getDefaultRenderer(Object.class);
        table.setDefaultRenderer(Object.class, new DarkTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
                    boolean hasFocus, int row, int column) {
                // remove left-to-right and right-to-left mark characters
                if (SystemInfo.isWindows) {
                    // on Windows 10, the date may contain left-to-right (0x200e) and right-to-left (0x200f)
                    // mark characters (see https://en.wikipedia.org/wiki/Left-to-right_mark)
                    // when the "current user" item is selected in the "look in" combobox
                    // --> remove them
                    if (value instanceof String str && str.startsWith("\u200e")) {
                        char[] buf = new char[str.length()];
                        int j = 0;
                        for (int i = 0; i < buf.length; i++) {
                            char ch = str.charAt(i);
                            if (ch != '\u200e' && ch != '\u200f')
                                buf[j++] = ch;
                        }
                        value = new String(buf, 0, j);
                    }
                }

                JLabel comp = (JLabel) super.getTableCellRendererComponent(
                        table, value, isSelected, hasFocus, row, column);
                JLabel defaultComp = (JLabel) defaultRenderer.getTableCellRendererComponent(
                        table, value, isSelected, hasFocus, row, column);

                comp.setIcon(defaultComp.getIcon());
                comp.setDisabledIcon(defaultComp.getDisabledIcon());
                comp.setText(defaultComp.getText());
                comp.setHorizontalAlignment(defaultComp.getHorizontalAlignment());
                return comp;
            }
        });
        table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        table.putClientProperty(DarkTableUI.KEY_FULL_ROW_FOCUS_BORDER, true);
        table.putClientProperty(DarkTableUI.KEY_FILE_CHOOSER_PARENT,
                (Supplier<JFileChooser>) this::getFileChooser);
        table.putClientProperty("JTable.fileNameColumnIndex", COLUMN_FILENAME);
        table.doLayout();
        int rowHeight = UIManager.getInt("FileChooser.rowHeight");
        table.addPropertyChangeListener(e -> {
            String name = e.getPropertyName();
            if ("rowHeight".equals(name)) {
                if (table.getRowHeight() != rowHeight) {
                    table.setRowHeight(rowHeight);
                }
            }
        });
        setupColumnModel(table.getColumnModel());
        table.addPropertyChangeListener(e -> {
            if ("columnModel".equals(e.getPropertyName())) {
                setupColumnModel(table.getColumnModel());
            }
        });
        table.setShowGrid(false);
    }

    private void setupColumnModel(TableColumnModel columnModel) {
        TableColumn column = columnModel.getColumn(COLUMN_FILENAME);
        column.setCellEditor(new FileTableEditor(column.getCellEditor()));
        column.addPropertyChangeListener(e -> {
            if ("cellEditor".equals(e.getPropertyName())) {
                if (!(column.getCellEditor() instanceof FileTableEditor)) {
                    column.setCellEditor(new FileTableEditor(column.getCellEditor()));
                }
            }
        });
    }

    @Override
    protected JPanel createList(final JFileChooser fc) {
        JPanel p = super.createList(fc);
        p = wrapInOverlayScrollPane(p);
        OverlayScrollPane sp = getComponentWithLayout(p, 0, OverlayScrollPane.class, null);
        if (sp != null) {
            Component cList = sp.getScrollPane().getViewport().getView();
            if (cList instanceof JList) {
                patchListView((JList<?>) cList);
            }
        }
        return p;
    }

    private void patchListView(final JList<?> list) {
        list.setCellRenderer(new DarkFileRenderer());
        list.putClientProperty(DarkListUI.KEY_FULL_ROW_SELECTION, true);
        list.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                Component c = e.getChild();
                if (c instanceof JTextComponent) {
                    c.addComponentListener(new ComponentAdapter() {
                        @Override
                        public void componentMoved(final ComponentEvent e) {
                            c.removeComponentListener(this);
                            ListCellRenderer<?> renderer = list.getCellRenderer();
                            if (renderer instanceof JLabel) {
                                int gap = ((JLabel) renderer).getIconTextGap() - 1;
                                if (c.getComponentOrientation().isLeftToRight()) {
                                    c.setBounds(c.getX() + gap, c.getY(),
                                            c.getWidth() - gap, c.getHeight());
                                } else {
                                    c.setBounds(c.getX(), c.getY(),
                                            c.getWidth() - gap, c.getHeight());
                                }
                            }
                        }
                    });
                    c.setName("FileChooser.listEditCell");
                    ((JTextComponent) c).setBorder(new TextTableCellEditorBorder());
                    ((JTextComponent) c).putClientProperty("JTextField.listCellEditor", true);
                    ((JTextComponent) c).putClientProperty(DarkTextUI.KEY_IS_LIST_EDITOR, true);
                }
            }
        });
    }


    private JPanel wrapInOverlayScrollPane(final JPanel panel) {
        JPanel p = panel;
        JScrollPane sp = getComponentWithLayout(p, 0, JScrollPane.class, null);
        if (sp != null) {
            p = new JPanel(new BorderLayout());
            OverlayScrollPane osp = new OverlayScrollPane(sp);
            p.setPreferredSize(UIManager.getDimension("FileChooser.prefContentSize"));
            osp.setAddHorizontalScrollBarSize(true);
            osp.setAddHorizontalScrollBarSize(true);
            p.add(osp);
        }
        return p;
    }

    private <T extends JComponent, L extends LayoutManager> T getComponentWithLayout(final Container parent,
            final int index,
            final Class<T> type, final Class<L> layoutType) {
        try {
            Component c = parent.getComponent(index);
            if (type.isInstance(c) && (layoutType == null || layoutType.isInstance(((JComponent) c).getLayout())))
                return type.cast(c);
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException(
                    "Invalid request: Parent=" + parent + ", index=" + index + ", layout=" + layoutType, e);
        }
        return null;
    }

    @Override
    public FileView getFileView(JFileChooser fc) {
        return fileView;
    }

    @Override
    public void clearIconCache() {
        fileView.clearIconCache();
    }

    protected DarkFileView createFileView() {
        return new DarkFileView();
    }

    private class DarkFileView extends BasicFileView {
        private static final String MIME_TEXT = "text/";
        private static final String MIME_IMAGE = "image/";

        @Override
        public Icon getIcon(final File f) {
            Icon icon = getCachedIcon(f);
            if (icon != null) {
                return icon;
            }
            icon = fileIcon;
            if (f != null) {
                FileSystemView fsv = getFileChooser().getFileSystemView();

                if (fsv.isFloppyDrive(f)) {
                    icon = floppyDriveIcon;
                } else if (fsv.isDrive(f)) {
                    icon = hardDriveIcon;
                } else if (fsv.isComputerNode(f)) {
                    icon = computerIcon;
                } else if (f.isDirectory()) {
                    icon = directoryIcon;
                } else {
                    try {
                        String mimeType = Files.probeContentType(f.toPath());
                        if (mimeType == null) mimeType = "";
                        if (mimeType.startsWith(MIME_IMAGE)) {
                            icon = imageFileIcon;
                        } else if (mimeType.startsWith(MIME_TEXT)) {
                            icon = textFileIcon;
                        }
                    } catch (final Exception e) {
                        icon = fileIcon;
                    }
                }
            }
            cacheIcon(f, icon);
            return icon;
        }
    }

    private class FileTableEditor extends DarkTableCellEditorDelegate {
        private FileTableEditor(final TableCellEditor delegate) {
            super(delegate);
            if (delegate instanceof DefaultCellEditor editor) {
                Component component = editor.getComponent();
                if (component instanceof JTextField tf) {
                    tf.removeAncestorListener(editorAncestorListener);
                    tf.addAncestorListener(editorAncestorListener);
                    setDelegate(new DarkTableCellEditor(tf) {
                        @Override
                        public Component getTableCellEditorComponent(JTable table, Object value,
                                boolean isSelected, int row, int column) {
                            Object realValue = value instanceof File ? getFileChooser().getName((File) value) : value;
                            return super.getTableCellEditorComponent(table, realValue, isSelected, row, column);
                        }
                    });
                }
            }
        }
    }

    private class DarkFileRenderer extends DefaultListCellRenderer {

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
}

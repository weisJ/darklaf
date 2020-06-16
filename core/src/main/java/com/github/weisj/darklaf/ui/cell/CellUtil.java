/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.github.weisj.darklaf.ui.cell;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.table.renderer.IconWrapper;
import com.github.weisj.darklaf.ui.tree.DarkTreeUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class CellUtil {

    public static final String KEY_SELECTED_CELL_RENDERER = "JComponent.selectedCellRenderer";

    // Default Colors
    private static Color cellForeground;
    private static Color cellForegroundSelected;
    private static Color cellForegroundNoFocus;
    private static Color cellForegroundSelectedNoFocus;

    private static Color cellInactiveForeground;
    private static Color cellInactiveForegroundSelected;
    private static Color cellInactiveForegroundNoFocus;
    private static Color cellInactiveForegroundSelectedNoFocus;

    private static Color cellBackground;
    private static Color cellBackgroundAlternative;
    private static Color cellBackgroundSelected;
    private static Color cellBackgroundNoFocus;
    private static Color cellBackgroundNoFocusAlternative;
    private static Color cellBackgroundSelectedNoFocus;

    private static Color cellInactiveBackground;
    private static Color cellInactiveBackgroundAlternative;
    private static Color cellInactiveBackgroundSelected;
    private static Color cellInactiveBackgroundNoFocus;
    private static Color cellInactiveBackgroundNoFocusAlternative;
    private static Color cellInactiveBackgroundSelectedNoFocus;

    // Table Colors
    private static Color tableCellForeground;
    private static Color tableCellForegroundSelected;
    private static Color tableCellForegroundNoFocus;
    private static Color tableCellForegroundSelectedNoFocus;

    private static Color tableCellInactiveForeground;
    private static Color tableCellInactiveForegroundSelected;
    private static Color tableCellInactiveForegroundNoFocus;
    private static Color tableCellInactiveForegroundSelectedNoFocus;

    private static Color tableCellBackground;
    private static Color tableCellBackgroundAlternative;
    private static Color tableCellBackgroundSelected;
    private static Color tableCellBackgroundNoFocus;
    private static Color tableCellBackgroundNoFocusAlternative;
    private static Color tableCellBackgroundSelectedNoFocus;

    private static Color tableCellInactiveBackground;
    private static Color tableCellInactiveBackgroundAlternative;
    private static Color tableCellInactiveBackgroundSelected;
    private static Color tableCellInactiveBackgroundNoFocus;
    private static Color tableCellInactiveBackgroundNoFocusAlternative;
    private static Color tableCellInactiveBackgroundSelectedNoFocus;

    // Tree Colors
    private static Color treeCellForeground;
    private static Color treeCellForegroundSelected;
    private static Color treeCellForegroundNoFocus;
    private static Color treeCellForegroundSelectedNoFocus;

    private static Color treeCellInactiveForeground;
    private static Color treeCellInactiveForegroundSelected;
    private static Color treeCellInactiveForegroundNoFocus;
    private static Color treeCellInactiveForegroundSelectedNoFocus;

    private static Color treeCellBackground;
    private static Color treeCellBackgroundAlternative;
    private static Color treeCellBackgroundSelected;
    private static Color treeCellBackgroundNoFocus;
    private static Color treeCellBackgroundNoFocusAlternative;
    private static Color treeCellBackgroundSelectedNoFocus;

    private static Color treeCellInactiveBackground;
    private static Color treeCellInactiveBackgroundAlternative;
    private static Color treeCellInactiveBackgroundSelected;
    private static Color treeCellInactiveBackgroundNoFocus;
    private static Color treeCellInactiveBackgroundNoFocusAlternative;
    private static Color treeCellInactiveBackgroundSelectedNoFocus;

    // List Colors
    private static Color listCellForeground;
    private static Color listCellForegroundSelected;
    private static Color listCellForegroundNoFocus;
    private static Color listCellForegroundSelectedNoFocus;

    private static Color listCellInactiveForeground;
    private static Color listCellInactiveForegroundSelected;
    private static Color listCellInactiveForegroundNoFocus;
    private static Color listCellInactiveForegroundSelectedNoFocus;

    private static Color listCellBackground;
    private static Color listCellBackgroundAlternative;
    private static Color listCellBackgroundSelected;
    private static Color comboListCellBackgroundSelected;
    private static Color listCellBackgroundNoFocus;
    private static Color listCellBackgroundNoFocusAlternative;
    private static Color listCellBackgroundSelectedNoFocus;

    private static Color listCellInactiveBackground;
    private static Color listCellInactiveBackgroundAlternative;
    private static Color listCellInactiveBackgroundSelected;
    private static Color listCellInactiveBackgroundNoFocus;
    private static Color listCellInactiveBackgroundNoFocusAlternative;
    private static Color listCellInactiveBackgroundSelectedNoFocus;

    public static void updateColors(final UIDefaults defaults) {
        UIDefaults d = defaults != null ? defaults : UIManager.getDefaults();
        // Default colors
        cellForeground = d.getColor("Cell.foreground");
        cellForegroundSelected = d.getColor("Cell.foregroundSelected");
        cellForegroundNoFocus = d.getColor("Cell.foregroundNoFocus");
        cellForegroundSelectedNoFocus = d.getColor("Cell.foregroundSelectedNoFocus");

        cellInactiveForeground = d.getColor("Cell.inactiveForeground");
        cellInactiveForegroundSelected = d.getColor("Cell.inactiveForegroundSelected");
        cellInactiveForegroundNoFocus = d.getColor("Cell.inactiveForegroundNoFocus");
        cellInactiveForegroundSelectedNoFocus = d.getColor("Cell.inactiveSelectedNoFocus");

        cellBackground = d.getColor("Cell.background");
        cellBackgroundAlternative = d.getColor("Cell.backgroundAlternative");
        cellBackgroundSelected = d.getColor("Cell.backgroundSelected");
        cellBackgroundNoFocus = d.getColor("Cell.backgroundNoFocus");
        cellBackgroundNoFocusAlternative = d.getColor("Cell.backgroundNoFocusAlternative");
        cellBackgroundSelectedNoFocus = d.getColor("Cell.backgroundSelectedNoFocus");

        cellInactiveBackground = d.getColor("Cell.inactiveBackground");
        cellInactiveBackgroundAlternative = d.getColor("Cell.inactiveBackgroundAlternative");
        cellInactiveBackgroundSelected = d.getColor("Cell.inactiveBackgroundSelected");
        cellInactiveBackgroundNoFocus = d.getColor("Cell.inactiveBackgroundNoFocus");
        cellInactiveBackgroundNoFocusAlternative = d.getColor("Cell.inactiveBackgroundNoFocusAlternative");
        cellInactiveBackgroundSelectedNoFocus = d.getColor("Cell.inactiveBackgroundSelectedNoFocus");

        // Table colors
        tableCellForeground = d.getColor("Table.foreground");
        tableCellForegroundSelected = d.getColor("Table.foregroundSelected");
        tableCellForegroundNoFocus = d.getColor("Table.foregroundNoFocus");
        tableCellForegroundSelectedNoFocus = d.getColor("Table.foregroundSelectedNoFocus");

        tableCellInactiveForeground = d.getColor("Table.inactiveForeground");
        tableCellInactiveForegroundSelected = d.getColor("Table.inactiveForegroundSelected");
        tableCellInactiveForegroundNoFocus = d.getColor("Table.inactiveForegroundNoFocus");
        tableCellInactiveForegroundSelectedNoFocus = d.getColor("Table.inactiveSelectedNoFocus");

        tableCellBackground = d.getColor("Table.background");
        tableCellBackgroundAlternative = d.getColor("Table.backgroundAlternative");
        tableCellBackgroundSelected = d.getColor("Table.backgroundSelected");
        tableCellBackgroundNoFocus = d.getColor("Table.backgroundNoFocus");
        tableCellBackgroundNoFocusAlternative = d.getColor("Table.backgroundNoFocusAlternative");
        tableCellBackgroundSelectedNoFocus = d.getColor("Table.backgroundSelectedNoFocus");

        tableCellInactiveBackground = d.getColor("Table.inactiveBackground");
        tableCellInactiveBackgroundAlternative = d.getColor("Table.inactiveBackgroundAlternative");
        tableCellInactiveBackgroundSelected = d.getColor("Table.inactiveBackgroundSelected");
        tableCellInactiveBackgroundNoFocus = d.getColor("Table.inactiveBackgroundNoFocus");
        tableCellInactiveBackgroundNoFocusAlternative = d.getColor("Table.inactiveBackgroundNoFocusAlternative");
        tableCellInactiveBackgroundSelectedNoFocus = d.getColor("Table.inactiveBackgroundSelectedNoFocus");

        // Tree colors
        treeCellForeground = d.getColor("Tree.foreground");
        treeCellForegroundSelected = d.getColor("Tree.foregroundSelected");
        treeCellForegroundNoFocus = d.getColor("Tree.foregroundNoFocus");
        treeCellForegroundSelectedNoFocus = d.getColor("Tree.foregroundSelectedNoFocus");

        treeCellInactiveForeground = d.getColor("Tree.inactiveForeground");
        treeCellInactiveForegroundSelected = d.getColor("Tree.inactiveForegroundSelected");
        treeCellInactiveForegroundNoFocus = d.getColor("Tree.inactiveForegroundNoFocus");
        treeCellInactiveForegroundSelectedNoFocus = d.getColor("Tree.inactiveSelectedNoFocus");

        treeCellBackground = d.getColor("Tree.background");
        treeCellBackgroundAlternative = d.getColor("Tree.backgroundAlternative");
        treeCellBackgroundSelected = d.getColor("Tree.backgroundSelected");
        treeCellBackgroundNoFocus = d.getColor("Tree.backgroundNoFocus");
        treeCellBackgroundNoFocusAlternative = d.getColor("Tree.backgroundNoFocusAlternative");
        treeCellBackgroundSelectedNoFocus = d.getColor("Tree.backgroundSelectedNoFocus");

        treeCellInactiveBackground = d.getColor("Tree.inactiveBackground");
        treeCellInactiveBackgroundAlternative = d.getColor("Tree.inactiveBackgroundAlternative");
        treeCellInactiveBackgroundSelected = d.getColor("Tree.inactiveBackgroundSelected");
        treeCellInactiveBackgroundNoFocus = d.getColor("Tree.inactiveBackgroundNoFocus");
        treeCellInactiveBackgroundNoFocusAlternative = d.getColor("Tree.inactiveBackgroundNoFocusAlternative");
        treeCellInactiveBackgroundSelectedNoFocus = d.getColor("Tree.inactiveBackgroundSelectedNoFocus");

        // List colors
        listCellForeground = d.getColor("List.foreground");
        listCellForegroundSelected = d.getColor("List.foregroundSelected");
        listCellForegroundNoFocus = d.getColor("List.foregroundNoFocus");
        listCellForegroundSelectedNoFocus = d.getColor("List.foregroundSelectedNoFocus");

        listCellInactiveForeground = d.getColor("List.inactiveForeground");
        listCellInactiveForegroundSelected = d.getColor("List.inactiveForegroundSelected");
        listCellInactiveForegroundNoFocus = d.getColor("List.inactiveForegroundNoFocus");
        listCellInactiveForegroundSelectedNoFocus = d.getColor("List.inactiveSelectedNoFocus");

        listCellBackground = d.getColor("List.background");
        listCellBackgroundAlternative = d.getColor("List.backgroundAlternative");
        listCellBackgroundSelected = d.getColor("List.backgroundSelected");
        comboListCellBackgroundSelected = d.getColor("ComboBox.selectionBackground");
        listCellBackgroundNoFocus = d.getColor("List.backgroundNoFocus");
        listCellBackgroundNoFocusAlternative = d.getColor("List.backgroundNoFocusAlternative");
        listCellBackgroundSelectedNoFocus = d.getColor("List.backgroundSelectedNoFocus");

        listCellInactiveBackground = d.getColor("List.inactiveBackground");
        listCellInactiveBackgroundAlternative = d.getColor("List.inactiveBackgroundAlternative");
        listCellInactiveBackgroundSelected = d.getColor("List.inactiveBackgroundSelected");
        listCellInactiveBackgroundNoFocus = d.getColor("List.inactiveBackgroundNoFocus");
        listCellInactiveBackgroundNoFocusAlternative = d.getColor("List.inactiveBackgroundNoFocusAlternative");
        listCellInactiveBackgroundSelectedNoFocus = d.getColor("List.inactiveBackgroundSelectedNoFocus");
    }

    public static void setupTableForeground(final Component comp, final JTable parent, final boolean selected) {
        setupForeground(comp, parent, selected,
                        tableCellForeground, tableCellForegroundSelected,
                        tableCellForegroundNoFocus, tableCellForegroundSelectedNoFocus,
                        tableCellInactiveForeground, tableCellInactiveForegroundSelected,
                        tableCellInactiveForegroundNoFocus, tableCellInactiveForegroundSelectedNoFocus);
    }

    public static void setupTreeForeground(final Component comp, final JTree parent, final boolean selected) {
        setupForeground(comp, parent, selected,
                        treeCellForeground, treeCellForegroundSelected,
                        treeCellForegroundNoFocus, treeCellForegroundSelectedNoFocus,
                        treeCellInactiveForeground, treeCellInactiveForegroundSelected,
                        treeCellInactiveForegroundNoFocus, treeCellInactiveForegroundSelectedNoFocus);
    }

    public static void setupListForeground(final Component comp, final JList<?> parent, final boolean selected) {
        setupForeground(comp, parent, selected,
                        listCellForeground, listCellForegroundSelected,
                        listCellForegroundNoFocus, listCellForegroundSelectedNoFocus,
                        listCellInactiveForeground, listCellInactiveForegroundSelected,
                        listCellInactiveForegroundNoFocus, listCellInactiveForegroundSelectedNoFocus);
    }

    public static void setupStandardForeground(final Component comp, final JComponent parent, final boolean selected) {
        setupForeground(comp, parent, selected,
                        cellForeground, cellForegroundSelected,
                        cellForegroundNoFocus, cellForegroundSelectedNoFocus,
                        cellInactiveForeground, cellInactiveForegroundSelected,
                        cellInactiveForegroundNoFocus, cellInactiveForegroundSelectedNoFocus);
    }

    public static void setupForeground(final Component comp, final JComponent parent, final boolean selected,
                                       final Color fg, final Color selFg,
                                       final Color fgNoFocus, final Color selFgNoFocus,
                                       final Color inactiveFg, final Color inactiveSelFg,
                                       final Color inactiveFgNoFocus, final Color inactiveSelFgNoFocus) {
        boolean enabled = comp.isEnabled() && parent.isEnabled();
        boolean focus = hasFocus(parent, comp);
        setupForeground(comp, parent, focus, selected, enabled, fg, selFg, fgNoFocus, selFgNoFocus, inactiveFg,
                        inactiveSelFg, inactiveFgNoFocus, inactiveSelFgNoFocus);
    }

    public static void setupForeground(final Component comp, final JComponent parent,
                                       final boolean focus, final boolean selected, final boolean enabled,
                                       final Color fg, final Color selFg,
                                       final Color fgNoFocus, final Color selFgNoFocus,
                                       final Color inactiveFg, final Color inactiveSelFg,
                                       final Color inactiveFgNoFocus, final Color inactiveSelFgNoFocus) {
        Color c = getColor(enabled, selected, focus,
                           fg, selFg, fgNoFocus, selFgNoFocus, inactiveFg, inactiveSelFg,
                           inactiveFgNoFocus, inactiveSelFgNoFocus);
        PropertyUtil.installForeground(comp, c);
    }

    public static void setupTableBackground(final Component comp, final JTable parent, final boolean selected,
                                            final int row) {
        boolean alt = row % 2 == 1 && PropertyUtil.getBooleanProperty(parent, DarkTableUI.KEY_ALTERNATE_ROW_COLOR);
        setupBackground(comp, hasFocus(parent, comp), selected,
                        alt ? tableCellBackgroundAlternative : tableCellBackground,
                        tableCellBackgroundSelected,
                        alt ? tableCellBackgroundNoFocusAlternative : tableCellBackgroundNoFocus,
                        tableCellBackgroundSelectedNoFocus,
                        alt ? tableCellInactiveBackgroundAlternative : tableCellInactiveBackground,
                        tableCellInactiveBackgroundSelected,
                        alt ? tableCellInactiveBackgroundNoFocusAlternative : tableCellInactiveBackgroundNoFocus,
                        tableCellInactiveBackgroundSelectedNoFocus);
    }

    public static void setupTreeBackground(final Component comp, final JTree parent, final boolean selected,
                                           final int row) {
        boolean alt = row % 2 == 1 && PropertyUtil.getBooleanProperty(parent, DarkTreeUI.KEY_ALTERNATE_ROW_COLOR);
        setupBackground(comp, hasFocus(parent, comp), selected,
                        alt ? treeCellBackgroundAlternative : treeCellBackground,
                        treeCellBackgroundSelected,
                        alt ? treeCellBackgroundNoFocusAlternative : treeCellBackgroundNoFocus,
                        treeCellBackgroundSelectedNoFocus,
                        alt ? treeCellInactiveBackgroundAlternative : treeCellInactiveBackground,
                        treeCellInactiveBackgroundSelected,
                        alt ? treeCellInactiveBackgroundNoFocusAlternative : treeCellInactiveBackgroundNoFocus,
                        treeCellInactiveBackgroundSelectedNoFocus);
    }

    public static Color getTreeBackground(final JTree tree, final boolean selected, final int row) {
        boolean alt = row % 2 == 1 && PropertyUtil.getBooleanProperty(tree, DarkTreeUI.KEY_ALTERNATE_ROW_COLOR);
        return getColor(tree.isEnabled(), selected, hasFocus(tree, tree),
                        alt ? treeCellBackgroundAlternative : treeCellBackground,
                        treeCellBackgroundSelected,
                        alt ? treeCellBackgroundNoFocusAlternative : treeCellBackgroundNoFocus,
                        treeCellBackgroundSelectedNoFocus,
                        alt ? treeCellInactiveBackgroundAlternative : treeCellInactiveBackground,
                        treeCellInactiveBackgroundSelected,
                        alt ? treeCellInactiveBackgroundNoFocusAlternative : treeCellInactiveBackgroundNoFocus,
                        treeCellInactiveBackgroundSelectedNoFocus);
    }

    public static void setupListBackground(final Component comp, final JList<?> parent, final boolean selected,
                                           final int index) {
        int layout = parent.getLayoutOrientation();
        boolean altRow = false;
        if (layout == JList.VERTICAL) {
            altRow = index % 2 == 1;
        } else if (layout == JList.VERTICAL_WRAP || layout == JList.HORIZONTAL_WRAP) {
            DarkListUI ui = DarkUIUtil.getUIOfType(parent.getUI(), DarkListUI.class);
            if (ui != null) {
                int row = ui.convertModelToRow(index);
                if (row == -1 && index >= parent.getModel().getSize()) {
                    row = ui.convertModelToRow(index - ui.getRowCount(ui.getColumnCount() - 1));
                }
                if (row == -1) {
                    row = index;
                }
                altRow = row % 2 == 1;
            } else {
                altRow = false;
            }
        }
        setupListBackground(comp, parent, selected, altRow);
    }

    public static void setupListBackground(final Component comp, final JList<?> parent, final boolean selected,
                                           final boolean altRow) {
        boolean alt = altRow && PropertyUtil.getBooleanProperty(parent, DarkListUI.KEY_ALTERNATE_ROW_COLOR);
        boolean comboList = PropertyUtil.getBooleanProperty(parent, DarkListUI.KEY_IS_COMBO_LIST);
        setupBackground(comp, hasFocus(parent, comp), selected,
                        alt ? listCellBackgroundAlternative : listCellBackground,
                        comboList ? comboListCellBackgroundSelected : listCellBackgroundSelected,
                        alt ? listCellBackgroundNoFocusAlternative : listCellBackgroundNoFocus,
                        listCellBackgroundSelectedNoFocus,
                        alt ? listCellInactiveBackgroundAlternative : listCellInactiveBackground,
                        listCellInactiveBackgroundSelected,
                        alt ? listCellInactiveBackgroundNoFocusAlternative : listCellInactiveBackgroundNoFocus,
                        listCellInactiveBackgroundSelectedNoFocus);
    }

    public static void setupStandardBackground(final Component comp, final JComponent parent, final boolean selected) {
        setupBackground(comp, hasFocus(parent, comp), selected,
                        cellBackground, cellBackgroundSelected,
                        cellBackgroundNoFocus, cellBackgroundSelectedNoFocus,
                        cellInactiveBackground, cellInactiveBackgroundSelected,
                        cellInactiveBackgroundNoFocus, cellInactiveBackgroundSelectedNoFocus);
    }

    public static void setupBackground(final Component comp, final boolean focus, final boolean selected,
                                       final Color bg, final Color selBg,
                                       final Color bgNoFocus, final Color selBgNoFocus,
                                       final Color inactiveBg, final Color inactiveSelBg,
                                       final Color inactiveBgNoFocus, final Color inactiveSelBgNoFocus) {
        boolean enabled = comp.isEnabled();
        Color c = getColor(enabled, selected, focus,
                           bg, selBg, bgNoFocus, selBgNoFocus, inactiveBg, inactiveSelBg,
                           inactiveBgNoFocus, inactiveSelBgNoFocus);
        PropertyUtil.installBackground(comp, c);
        if (comp instanceof JComponent) {
            LookAndFeel.installProperty((JComponent) comp, PropertyKey.OPAQUE, true);
        }
    }

    public static Color getColor(final boolean enabled, final boolean selected, final boolean focus,
                                 final Color color, final Color selColor,
                                 final Color colorNoFocus, final Color selColorNoFocus,
                                 final Color inactiveColor, final Color inactiveSelColor,
                                 final Color inactiveColorNoFocus, final Color inactiveSelColorNoFocus) {
        Color c;
        if (enabled) {
            if (selected) {
                if (focus) {
                    c = selColor;
                } else {
                    c = selColorNoFocus;
                }
            } else {
                if (focus) {
                    c = color;
                } else {
                    c = colorNoFocus;
                }
            }
        } else {
            if (selected) {
                if (focus) {
                    c = inactiveSelColor;
                } else {
                    c = inactiveSelColorNoFocus;
                }
            } else {
                if (focus) {
                    c = inactiveColor;
                } else {
                    c = inactiveColorNoFocus;
                }
            }
        }
        return c;
    }

    protected static boolean hasFocus(final Component c, final Component comp) {
        return comp.hasFocus()
               || (DarkUIUtil.hasFocus(c) || DarkUIUtil.getParentOfType(JPopupMenu.class, c, 4) != null);
    }

    public static void setSelectedFlag(final Component comp, final boolean selected) {
        if (comp instanceof JComponent) {
            ((JComponent) comp).putClientProperty(KEY_SELECTED_CELL_RENDERER, selected);
        }
    }

    public static void paintTableEditorBorder(final Graphics g, final Component c, final JTable table,
                                              final int width, final int height) {
        int row = table.getEditingRow();
        int col = table.getEditingColumn();
        if (!table.getShowHorizontalLines()) {
            if (row > CellUtil.getMinRowIndex(table)) g.fillRect(0, 0, width, 1);
            g.fillRect(0, height - 1, width, 1);
        }
        if (!table.getShowVerticalLines()) {
            if (col > CellUtil.getMinColumnIndex(table)) g.fillRect(0, 0, 1, height);
            if (col < CellUtil.getMaxColumnIndex(table)) g.fillRect(width - 1, 0, 1, height);
        } else if (isInWrapper(c)) {
            if (table.getComponentOrientation().isLeftToRight()) {
                g.fillRect(0, 0, 1, height);
            } else {
                g.fillRect(width - 1, 0, 1, height);
            }
        }
    }

    protected static boolean isInWrapper(final Component c) {
        return c.getParent() instanceof IconWrapper;
    }

    protected static boolean isListEditor(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkListUI.KEY_IS_LIST_EDITOR) && c.getParent() instanceof JList;
    }

    public static Insets adjustEditorInsets(final Insets ins, final Component c) {
        return adjustEditorInsets(ins, c, DarkUIUtil.getParentOfType(JTable.class, c, 2));
    }

    public static Insets adjustEditorInsets(final Insets ins, final Component c, final JTable table) {
        if (isInWrapper(c)) {
            if (parentLTR(c)) {
                ins.left -= ((IconWrapper) c.getParent()).getIconCompGap();
            } else {
                ins.right -= ((IconWrapper) c.getParent()).getIconCompGap();
            }
        } else if (isListEditor(c)) {
            ListCellRenderer<?> renderer = ((JList<?>) c.getParent()).getCellRenderer();
            if (renderer instanceof JLabel) {
                if (parentLTR(c)) {
                    ins.left -= ((JLabel) renderer).getIconTextGap() - 1;
                } else {
                    ins.right -= ((JLabel) renderer).getIconTextGap() - 1;
                }
            }
        }
        return adjustTableCellEditorInsets(ins, table);
    }

    public static Insets adjustTableCellEditorInsets(final Insets ins, final JTable table) {
        if (table != null && !table.getShowVerticalLines()) {
            int cMin = getMinColumnIndex(table);
            int column = table.getEditingColumn();
            if (column > cMin) ins.left++;
        }
        return ins;
    }

    protected static boolean parentLTR(final Component c) {
        return c.getParent().getComponentOrientation().isLeftToRight();
    }

    public static int getMinColumnIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        return table.columnAtPoint(rect.getLocation());
    }

    public static int getMaxColumnIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        Point p = rect.getLocation();
        p.x += rect.width - 1;
        return table.columnAtPoint(p);
    }

    public static int getMinRowIndex(final JTable table) {
        Rectangle rect = table.getVisibleRect();
        return table.rowAtPoint(rect.getLocation());
    }
}

package com.weis.darklaf.ui.table;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

/**
 * @author vincencopalazzo
 * @author atarw
 */
public class DarkTableCellRendererCheckBox extends JCheckBox implements TableCellRenderer, SwingConstants {

    private boolean hasFocus;

    public DarkTableCellRendererCheckBox() {
        this(false);
    }


    public DarkTableCellRendererCheckBox(final boolean selected) {
        setSelected(selected);
    }

    @Override
    public boolean hasFocus() {
        return hasFocus || super.hasFocus();
    }

    @Override
    public boolean isFocusOwner() {
        return super.hasFocus();
    }

    @Override
    public Component getTableCellRendererComponent(final JTable table, final Object value,
                                                   final boolean isSelected, final boolean focus,
                                                   final int row, final int column) {
        if (value instanceof Boolean) {
            setSelected((Boolean) value);
        }
        setHorizontalAlignment(table.getComponentOrientation().isLeftToRight() ? LEFT : RIGHT);
        hasFocus = focus;

        boolean alternativeRow = UIManager.getBoolean("Table.alternateRowColor");
        Color alternativeRowColor = UIManager.getColor("Table.alternateRowBackground");
        Color normalColor = UIManager.getColor("Table.background");
        if (alternativeRow) {
            if (!isSelected) {
                if (row % 2 == 1) {
                    this.setBackground(alternativeRowColor);
                } else {
                    this.setBackground(normalColor);
                }
                this.setForeground(table.getForeground());
            } else {
                this.setForeground(table.getSelectionForeground());
                this.setBackground(table.getSelectionBackground());
            }
        }
        return this;
    }
}

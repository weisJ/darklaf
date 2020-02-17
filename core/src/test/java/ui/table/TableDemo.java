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
 */
package ui.table;

import com.github.weisj.darklaf.ui.table.DarkTableCellEditor;
import ui.ComponentDemo;
import ui.DemoPanel;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import java.awt.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class TableDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TableDemo());
    }

    @Override
    public JComponent createComponent() {
        String[] columns = new String[]{
            "Id", "Name", "Hourly Rate", "Part Time", "Components"
        };

        Object[][] data = new Object[][]{
            {1, "John", 40.0, false, "Item"},
            {2, "Rambo", 70.0, false, 10},
            {3, "Zorro", 60.0, true, "cell"},
        };
        AtomicBoolean editable = new AtomicBoolean(true);
        JTable table = new JTable(data, columns) {
            TableCellEditor comboEditor = new DarkTableCellEditor(new JComboBox<>());
            TableCellEditor spinnerEditor = new DarkTableCellEditor(new JSpinner());

            @Override
            public boolean isCellEditable(final int row, final int column) {
                return editable.get() && super.isCellEditable(row, column);
            }

            @Override
            public TableCellEditor getCellEditor(final int row, final int column) {
                if (row == 0 && column == 4) {
                    return comboEditor;
                } else if (row == 1 && column == 4) {
                    return spinnerEditor;
                } else {
                    return super.getCellEditor(row, column);
                }
            }
        };

        JTableHeader header = table.getTableHeader();
        DemoPanel panel = new DemoPanel(new JScrollPane(table));

        JPanel controlPanel = panel.getControls();
        controlPanel.setLayout(new GridLayout(4, 2));
        controlPanel.add(new JCheckBox("enabled") {{
            setSelected(table.isEnabled());
            addActionListener(e -> table.setEnabled(isSelected()));
        }});
        controlPanel.add(new JCheckBox("editable") {{
            setSelected(editable.get());
            addActionListener(e -> editable.set(isSelected()));
        }});
        controlPanel.add(new JCheckBox("horizontal lines") {{
            setSelected(table.getShowHorizontalLines());
            addActionListener(e -> table.setShowHorizontalLines(isSelected()));
        }});
        controlPanel.add(new JCheckBox("vertical lines") {{
            setSelected(table.getShowVerticalLines());
            addActionListener(e -> table.setShowVerticalLines(isSelected()));
        }});
        controlPanel.add(new JCheckBox("LeftToRight") {{
            setSelected(table.getComponentOrientation().isLeftToRight());
            addActionListener(e -> table.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                                                                              : ComponentOrientation.RIGHT_TO_LEFT));
        }});
        controlPanel.add(new JCheckBox("reordering") {{
            setSelected(header.getReorderingAllowed());
            addActionListener(e -> header.setReorderingAllowed(isSelected()));
        }});
        JRadioButton column = new JRadioButton("column selection allowed") {{
            setSelected(table.getColumnSelectionAllowed());
            addActionListener(e -> {
                table.setColumnSelectionAllowed(isSelected());
                table.setRowSelectionAllowed(!isSelected());
            });
        }};
        JRadioButton row = new JRadioButton("row selection allowed") {{
            setSelected(table.getRowSelectionAllowed());
            addActionListener(e -> {
                table.setRowSelectionAllowed(isSelected());
                table.setColumnSelectionAllowed(!isSelected());
            });
        }};
        ButtonGroup group = new ButtonGroup();
        group.add(column);
        group.add(row);
        controlPanel.add(column);
        controlPanel.add(row);
        return panel;
    }

    @Override
    public String getTitle() {
        return "Table Demo";
    }
}

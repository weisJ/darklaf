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
package defaults;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;

import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;

/*
 * Render the value based on its class.
 */
public class SampleRenderer extends JLabel implements TableCellRenderer {

    private final DarkCellRendererToggleButton<?> booleanRenderer = new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellCheckBox(true));

    public SampleRenderer() {
        super();
        setHorizontalAlignment(SwingConstants.CENTER);
        setOpaque(true);
    }

    public Component getTableCellRendererComponent(final JTable table,
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
        } else if (sample instanceof Boolean) {
            return booleanRenderer.getTableCellRendererComponent(table, sample, isSelected, hasFocus, row, column);
        }
        return this;
    }

    /*
     * Some icons are painted using inner classes and are not meant to be
     * shared by other items. This code will catch the
     * ClassCastException that is thrown.
     */
    public void paint(final Graphics g) {
        try {
            super.paint(g);
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
}

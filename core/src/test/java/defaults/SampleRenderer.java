package defaults;

import com.github.weisj.darklaf.ui.cell.DarkCellRendererToggleButton;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

/*
 *  Render the value based on its class.
 */
public class SampleRenderer extends JLabel implements TableCellRenderer {

    private final DarkCellRendererToggleButton<?> booleanRenderer =
        new DarkCellRendererToggleButton<>(new DarkCellRendererToggleButton.CellEditorCheckBox(true));

    public SampleRenderer() {
        super();
        setHorizontalAlignment(SwingConstants.CENTER);
        setOpaque(true);
    }


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
        } else if (sample instanceof Boolean) {
            return booleanRenderer.getTableCellRendererComponent(table, sample, isSelected, hasFocus, row, column);
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
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
}

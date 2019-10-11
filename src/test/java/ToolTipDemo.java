import com.weis.darklaf.LafManager;
import com.weis.darklaf.components.tooltip.ToolTipContext;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

public class ToolTipDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JFrame f = new JFrame();
            var p = new JPanel();
            p.add(new JButton("Button with very very long text") {
                private final ToolTipContext context = new ToolTipContext(this);

                {
                    setToolTipText("ToolTip \n multiline \n third line's a charm");
                }

                @Override
                protected void paintComponent(final Graphics g) {
                    super.paintComponent(g);
                    g.setColor(Color.RED);
                    g.fillRect(getWidth() / 2, getHeight() / 2, 1, 1);
                }

                @Override
                public JToolTip createToolTip() {
                    return context.getToolTip();
                }

                @Override
                public Point getToolTipLocation(final MouseEvent event) {
                    return context.getToolTipLocation(event);
                }


            });
            f.setContentPane(p);
            f.setLocationRelativeTo(null);
            f.setSize(100, 100);
            f.setVisible(true);
        });
    }
}

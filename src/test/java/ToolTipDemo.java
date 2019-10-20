import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

public class ToolTipDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JFrame f = new JFrame();
            var p = new JPanel(new GridBagLayout());
            p.add(new JButton("Button with very very long text") {
                private final ToolTipContext context = new ToolTipContext(this).setAlignment(Alignment.CENTER)
                                                                               .setCenterAlignment(Alignment.SOUTH_EAST);

                {
                    setToolTipText("ToolTip \n multiline \n third line's a charm");
//                    setToolTipText("ToolTip");
                }

                @Override
                protected void paintComponent(final Graphics g) {
                    super.paintComponent(g);
                    g.setColor(Color.RED);
                    g.fillRect(getWidth() / 2, getHeight() / 2, 1, 1);
                }

                @Override
                public Point getToolTipLocation(final MouseEvent event) {
                    return context.getToolTipLocation(event);
                }

                @Override
                public JToolTip createToolTip() {
                    return context.getToolTip();
                }


            });
            f.setContentPane(p);
            f.setLocationRelativeTo(null);
            f.setSize(100, 100);
            f.setVisible(true);
        });
    }
}

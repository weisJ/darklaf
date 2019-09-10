import com.weis.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public class TabbedPaneDemo extends JFrame {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);
            final JFrame frame = new JFrame();
            frame.setSize(500, 500);
            var p = new JPanel(new BorderLayout());
            final var tabbedPane = new JTabbedPane();
            for (int i = 0; i < 20; i++) {
                var editor = new JTextPane();
                editor.setText("mima.TabFrameDemo\n".repeat(i + 1));
                tabbedPane.addTab("Tab number " + i, editor);
            }
//            tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
            p.add(tabbedPane, BorderLayout.CENTER);
            p.add(new JPanel(){{
                setLayout(new FlowLayout(FlowLayout.LEFT));
                add(new JButton("Unfocus"));
            }}, BorderLayout.SOUTH);
            frame.setContentPane(p);
            frame.setLocationRelativeTo(null);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
            frame.repaint();
        });
    }
}


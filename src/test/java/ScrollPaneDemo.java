import com.weis.darklaf.LafManager;
import org.jdesktop.swingx.MultiSplitLayout;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;

/**
 * @author Jannis Weis
 * @since 2019
 */
public final class ScrollPaneDemo extends MultiSplitLayout {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            final var frame = new JFrame();
            frame.setSize(500, 500);
            var overlayScroll = new JScrollPane(new JEditorPane() {{
                setEditorKit(new HTMLEditorKit());
                setText(TestResources.LOREM_IPSUM);
                setFont(Font.getFont(Font.MONOSPACED));
            }});
            frame.setContentPane(new JPanel(new BorderLayout()) {{
                add(overlayScroll, BorderLayout.CENTER);
            }});
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }
}

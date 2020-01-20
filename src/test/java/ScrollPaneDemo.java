import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.util.StringUtil;
import org.jdesktop.swingx.MultiSplitLayout;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 * @since 2019
 */
public final class ScrollPaneDemo extends MultiSplitLayout {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            final JFrame frame = new JFrame();
            frame.setSize(500, 500);
            JTextPane overlayScroll = new JTextPane() {{
                setText(StringUtil.repeat(TestResources.LOREM_IPSUM, 10));
                setFont(Font.getFont(Font.MONOSPACED));
//                setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
//                SimpleAttributeSet attribs = new SimpleAttributeSet();
//                StyleConstants.setAlignment(attribs, StyleConstants.ALIGN_RIGHT);
//                setParagraphAttributes(attribs, true);
            }};
            frame.setContentPane(new JPanel(new BorderLayout()) {{
                add(overlayScroll, BorderLayout.CENTER);
            }});
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }
}

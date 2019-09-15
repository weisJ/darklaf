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
            LafManager.loadLaf(LafManager.Theme.Dark);
            final var frame = new JFrame();
            frame.setSize(500, 500);
            var overlayScroll = new JScrollPane(new JEditorPane() {{
                setEditorKit(new HTMLEditorKit());
                setText("<body>Lorem ipsum dolor sit amet, consectetur adipiscing elit. In tempor quis nibh a semper. Nullam"
                        + " auctor, erat non viverra commodo, libero orci aliquam quam, ac interdum nunc est sed "
                        + "ligula. Aliquam vel velit non dolor accumsan blandit id eu metus. Aenean iaculis urna in "
                        + "placerat aliquam. Aliquam dui quam, bibendum sed magna in, cursus ornare est. Quisque "
                        + "tempor nunc quis nunc tempor convallis. Vestibulum tristique luctus ante, ac hendrerit dui"
                        + ".\n"
                        + "\n"
                        + "Donec ut maximus augue. Nam eleifend maximus scelerisque. Duis varius accumsan est, non "
                        + "aliquam dolor. Aenean iaculis nibh in aliquam viverra. Sed laoreet, urna ut facilisis "
                        + "convallis, arcu turpis vestibulum augue, id convallis tellus metus nec orci. Lorem ipsum "
                        + "dolor sit amet, consectetur adipiscing elit. Donec hendrerit purus velit, at blandit elit "
                        + "luctus ut. Proin diam nisl, sodales vitae dignissim nec, eleifend eu libero. Maecenas odio"
                        + " ligula, fermentum eget nisl vel, cursus tristique est. In nec nibh nec dui tempor "
                        + "ullamcorper. Praesent tincidunt luctus sem, ut luctus dolor commodo non. Nulla consectetur"
                        + " facilisis dolor, in facilisis ligula fringilla et. Cras id placerat libero. Donec "
                        + "vehicula orci a quam rutrum, eu efficitur lorem iaculis. Aenean varius nisi in dictum "
                        + "accumsan.\n"
                        + "\n"
                        + "Nulla massa ipsum, consectetur non gravida ut, blandit quis velit. Ut pretium quam aliquam"
                        + " diam porttitor mattis. Nam ullamcorper, felis ut iaculis iaculis, nunc odio pulvinar "
                        + "enim, vitae iaculis turpis sapien iaculis metus. Donec rutrum varius augue in dictum. Cras"
                        + " vestibulum vitae mauris ut finibus. Ut dictum imperdiet lorem et imperdiet. Vivamus "
                        + "semper tempor dolor eu porta. Sed at vehicula nisl. Pellentesque ut lorem tincidunt, "
                        + "elementum ligula at, porta turpis. Praesent feugiat dolor diam, at facilisis metus gravida"
                        + " non. Aliquam quis pellentesque nibh. Sed vestibulum porttitor nisi. In vitae malesuada "
                        + "sapien.");
//                setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
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

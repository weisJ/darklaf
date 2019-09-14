import com.weis.darklaf.icons.IconLoader;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class DnDTest extends JFrame {

    public DnDTest() {

        initUI();
    }

    private void initUI() {

        var icon1 = IconLoader.get().getUIAwareIcon("files/folder.svg");
        var icon2 = IconLoader.get().getUIAwareIcon("files/text.svg");
        var icon3 = IconLoader.get().getUIAwareIcon("files/unknown.svg");

        var label1 = new JLabel(icon1, JLabel.CENTER);
        var label2 = new JLabel(icon2, JLabel.CENTER);
        var label3 = new JLabel(icon3, JLabel.CENTER);

        var listener = new DragMouseAdapter();
        label1.addMouseListener(listener);
        label2.addMouseListener(listener);
        label3.addMouseListener(listener);

        var button = new JButton(icon2);
        button.setFocusable(false);

        label1.setTransferHandler(new TransferHandler("icon"));
        label2.setTransferHandler(new TransferHandler("icon"));
        label3.setTransferHandler(new TransferHandler("icon"));
        button.setTransferHandler(new TransferHandler("icon"));

        createLayout(label1, label2, label3, button);

        setTitle("Icon Drag & Drop");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
    }

    private class DragMouseAdapter extends MouseAdapter {

        public void mousePressed(MouseEvent e) {

            var c = (JComponent) e.getSource();
            var handler = c.getTransferHandler();
            handler.exportAsDrag(c, e, TransferHandler.COPY);
        }
    }

    private void createLayout(JComponent... arg) {

        var pane = getContentPane();
        var gl = new GroupLayout(pane);
        pane.setLayout(gl);

        gl.setAutoCreateContainerGaps(true);
        gl.setAutoCreateGaps(true);

        gl.setHorizontalGroup(gl.createParallelGroup(GroupLayout.Alignment.CENTER)
                                .addGroup(gl.createSequentialGroup()
                                            .addComponent(arg[0])
                                            .addGap(30)
                                            .addComponent(arg[1])
                                            .addGap(30)
                                            .addComponent(arg[2])
                                )
                                .addComponent(arg[3], GroupLayout.DEFAULT_SIZE,
                                              GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        );

        gl.setVerticalGroup(gl.createSequentialGroup()
                              .addGroup(gl.createParallelGroup()
                                          .addComponent(arg[0])
                                          .addComponent(arg[1])
                                          .addComponent(arg[2]))
                              .addGap(30)
                              .addComponent(arg[3])
        );

        pack();
    }

    public static void main(String[] args) {

        EventQueue.invokeLater(() -> {

            var ex = new DnDTest();
            ex.setVisible(true);
        });
    }
}

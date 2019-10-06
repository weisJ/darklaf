import javax.swing.*;

public class MyInternalFrame extends JInternalFrame {
    private static int openFrameCount = 0;
    private static final int xOffset = 30, yOffset = 30;

    public MyInternalFrame() {
        super("Document #" + (++openFrameCount), true, true, true, true);
        setSize(300, 300);
        setLocation(xOffset * openFrameCount, yOffset * openFrameCount);
        setJMenuBar(new JMenuBar() {{
            add(new JMenu("Test") {{
                add(new JMenuItem("Test Item"));
            }});
        }});
    }
}

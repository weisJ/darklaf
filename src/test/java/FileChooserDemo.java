import com.weis.darklaf.LafManager;

import javax.swing.*;

public final class FileChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            var chooser = new JFileChooser(System.getProperty("user.home"));
            chooser.setMultiSelectionEnabled(true);
            var frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
            chooser.showOpenDialog(frame);
        });
    }
}

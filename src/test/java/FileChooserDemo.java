import com.github.weisj.darklaf.LafManager;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.File;

public final class FileChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JFileChooser chooser = new JFileChooser(System.getProperty("user.home"));
            FileNameExtensionFilter filter = new FileNameExtensionFilter("JSON files", "json");
            chooser.addChoosableFileFilter(new FileFilter() {
                @Override
                public boolean accept(final File f) {
                    return f.isDirectory() || filter.accept(f);
                }

                @Override
                public String getDescription() {
                    return filter.getDescription();
                }
            });
            chooser.setMultiSelectionEnabled(true);
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
            chooser.showOpenDialog(frame);
        });
    }
}

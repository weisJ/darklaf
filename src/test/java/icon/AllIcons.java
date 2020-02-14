/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package icon;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.ThemedSVGIcon;
import com.github.weisj.darklaf.util.Pair;
import com.kitfox.svg.app.beans.SVGIcon;

import javax.swing.*;
import javax.swing.event.ListDataListener;
import java.awt.*;
import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class AllIcons {

    private static String[] FOLDERS = new String[]{
            "control", "dialog", "files", "indicator", "menu", "misc", "navigation", "window",
    };

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            try {
                JFrame frame = new JFrame("Icons");
                JList<Pair<String, Icon>> list = new JList<>(new ListModel<Pair<String, Icon>>() {
                    List<Pair<String, Icon>> elements = loadIcons();

                    @Override
                    public int getSize() {
                        return elements.size();
                    }

                    @Override
                    public Pair<String, Icon> getElementAt(final int index) {
                        return elements.get(index);
                    }

                    @Override
                    public void addListDataListener(final ListDataListener l) {
                    }

                    @Override
                    public void removeListDataListener(final ListDataListener l) {
                    }
                });
                list.setLayoutOrientation(JList.VERTICAL);
                list.setCellRenderer(new IconListRenderer());
                frame.setContentPane(new OverlayScrollPane(list));
                frame.pack();
                frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            } catch (URISyntaxException e) {
                e.printStackTrace();
            }
        });
    }


    private static List<Pair<String, Icon>> loadIcons() throws URISyntaxException {
        List<Pair<String, Icon>> list = new ArrayList<>();
        for (String folder : FOLDERS) {
            File[] files = getResourceFolderFiles("icons/" + folder, DarkLaf.class);
            for (File f : files) {
                if (f.getName().endsWith(".svg")) {
                    int SIZE = 30;
                    ThemedSVGIcon icon = (ThemedSVGIcon) IconLoader.get().loadSVGIcon(folder + "/" + f.getName(), SIZE, SIZE, true);
                    SVGIcon svgIcon = icon.getSVGIcon();
                    boolean scale = svgIcon.isScaleToFit();
                    svgIcon.setScaleToFit(false);
                    icon.setDisplaySize(svgIcon.getIconWidth() * 2, svgIcon.getIconHeight() * 2);
                    svgIcon.setScaleToFit(scale);
                    list.add(new Pair<>(f.getName(), icon));
                }
            }
        }
        return list;
    }

    private static File[] getResourceFolderFiles(final String folder,
                                                 final Class<?> clazz) throws URISyntaxException {
        URL url = clazz.getResource(folder);
        return new File(url.toURI()).listFiles();
    }

    private static final class IconListRenderer extends JLabel implements ListCellRenderer<Pair<String, Icon>> {

        @Override
        public Component getListCellRendererComponent(final JList<? extends Pair<String, Icon>> list,
                                                      final Pair<String, Icon> value, final int index,
                                                      final boolean isSelected, final boolean cellHasFocus) {
            setIcon(value.getSecond());
            setText(value.getFirst());
            return this;
        }
    }
}

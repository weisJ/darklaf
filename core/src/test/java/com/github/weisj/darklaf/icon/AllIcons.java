/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.icon;

import java.awt.*;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

import javax.swing.*;
import javax.swing.event.ListDataListener;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.platform.decorations.DecorationsProvider;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.properties.icons.ThemedSVGIcon;
import com.github.weisj.darklaf.ui.ComponentDemo;
import com.github.weisj.darklaf.util.ClassFinder;
import com.github.weisj.darklaf.util.Instantiable;
import com.github.weisj.darklaf.util.Lambdas;
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.ResourceWalker;

public class AllIcons implements ComponentDemo {

    private static final int ICON_SIZE = 50;

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new AllIcons());
    }

    public AllIcons() {
        List<DecorationsProvider> decorationsProviders =
                ClassFinder.getInstancesOfType(DecorationsProvider.class, "com.github.weisj.darklaf")
                        .stream()
                        .map(Lambdas.orDefault(Instantiable::instantiate, null))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
        LafManager.registerInitTask((currentTheme, defaults) -> {
            Properties props = new Properties();
            decorationsProviders.forEach(provider -> provider.loadDecorationProperties(props, defaults));
            defaults.putAll(props);
        });
    }

    @Override
    public JComponent createComponent() {
        return new OverlayScrollPane(createIconJList(ICON_SIZE));
    }

    protected static JList<Pair<String, ? extends Icon>> createIconJList(final int displaySize) {
        JList<Pair<String, ? extends Icon>> list = new JList<>(new ListModel<Pair<String, ? extends Icon>>() {
            final List<Pair<String, ? extends Icon>> elements = loadIcons(displaySize, true);

            @Override
            public int getSize() {
                return elements.size();
            }

            @Override
            public Pair<String, ? extends Icon> getElementAt(final int index) {
                return elements.get(index);
            }

            @Override
            public void addListDataListener(final ListDataListener l) {}

            @Override
            public void removeListDataListener(final ListDataListener l) {}
        });
        list.setLayoutOrientation(JList.VERTICAL);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.setCellRenderer(new IconListRenderer());
        return list;
    }

    protected static List<Pair<String, ? extends Icon>> loadIcons(final int displaySize, final boolean centered) {
        IconLoader loader = IconLoader.get();
        try (ResourceWalker walker = ResourceWalker.walkResources("com.github.weisj")) {
            return walker.stream().filter(p -> p.endsWith("svg")).map(p -> {
                ThemedSVGIcon icon = (ThemedSVGIcon) loader.loadSVGIcon(p, -displaySize, -displaySize, true);
                return new Pair<>(p, centered ? new CenterIcon(icon, displaySize, displaySize) : icon);
            }).collect(Collectors.groupingBy(pair -> pathToIconName(pair.getFirst())))
                    .values().stream()
                    .peek(list -> makeUnique(list, 1))
                    .flatMap(List::stream)
                    .sorted(Pair.compareFirst(AllIcons::pathToIconName)).collect(Collectors.toList());
        }
    }

    private static <T> void makeUnique(final List<Pair<String, T>> iconList, final int depth) {
        if (iconList.size() <= 1) {
            iconList.forEach(p -> p.setFirst(pathToIconName(p.getFirst(), depth)));
        } else {
            iconList.stream()
                    .collect(Collectors.groupingBy(p -> pathToIconName(p.getFirst(), depth + 1)))
                    .values()
                    .forEach(list -> makeUnique(list, depth + 1));
        }
    }

    private static String pathToIconName(final String p) {
        return pathToIconName(p, 1);
    }

    private static String pathToIconName(final String p, final int subPathLength) {
        int index = p.length();
        for (int i = 0; i < subPathLength; i++) {
            index = p.lastIndexOf('/', index - 1);
        }
        return p.substring(index + 1);
    }

    @Override
    public String getTitle() {
        return "All Icons";
    }

    private static final class IconListRenderer extends JLabel
            implements ListCellRenderer<Pair<String, ? extends Icon>> {

        private IconListRenderer() {
            setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        }

        @Override
        public Component getListCellRendererComponent(final JList<? extends Pair<String, ? extends Icon>> list,
                final Pair<String, ? extends Icon> value, final int index, final boolean isSelected,
                final boolean cellHasFocus) {
            setIcon(value.getSecond());
            setText(value.getFirst());
            return this;
        }
    }

    private static class CenterIcon implements Icon {

        private final Icon icon;
        private final int width;
        private final int height;

        private CenterIcon(final Icon icon, final int width, final int height) {
            this.icon = icon;
            this.width = width;
            this.height = height;
        }

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            int px = x + (width - icon.getIconWidth()) / 2;
            int py = y + (height - icon.getIconHeight()) / 2;
            icon.paintIcon(c, g, px, py);
        }

        @Override
        public int getIconWidth() {
            return width;
        }

        @Override
        public int getIconHeight() {
            return height;
        }
    }
}

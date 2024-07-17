/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.scrollPane;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.swing.*;
import javax.swing.text.*;

import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rtextarea.RTextScrollPane;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.graphics.SizedPainter;
import com.github.weisj.darklaf.theme.ColorPalette;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.DemoResources;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.scrollpane.ScrollBarConstants;
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.StringUtil;

public class OverlayRSyntaxScrollPane extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new OverlayRSyntaxScrollPane());
    }

    @Override
    public JComponent createComponent() {
        RSyntaxTextArea textArea = new RSyntaxTextArea(StringUtil.repeat(DemoResources.LOREM_IPSUM, 5));
        RTextScrollPane sp = new RTextScrollPane(textArea);
        OverlayScrollPane scrollPane = new OverlayScrollPane(sp);

        Random r = new Random();
        Color[] colors = new Color[] {ColorPalette.ORANGE, ColorPalette.RED, ColorPalette.GREEN};
        int length = textArea.getDocument().getLength();
        List<Pair<Position, Color>> markers = new ArrayList<>();
        int count = 15;
        for (int i = 0; i < count; i++) {
            try {
                Position position = textArea.getDocument().createPosition(i * length / count);
                Color color = colors[r.nextInt(colors.length)];
                markers.add(new Pair<>(position, color));
            } catch (BadLocationException ignored) {
            }
        }

        scrollPane.getVerticalScrollBar().putClientProperty(ScrollBarConstants.KEY_BACKGROUND_PAINTER,
                new MarkerBackgroundPainter(textArea, markers));
        return new DemoPanel(scrollPane, new BorderLayout(), 0);
    }

    @Override
    public String getName() {
        return "OverlayRSyntaxScrollPane Demo";
    }

    private record MarkerBackgroundPainter(JTextComponent textComponent,
            List<Pair<Position, Color>> markers) implements SizedPainter<JScrollBar> {

        @Override
        public void paint(final Graphics2D g, final JScrollBar object, final int width, final int height) {
            int totalHeight = textComponent.getHeight();
            for (Pair<Position, Color> marker : markers) {
                Position position = marker.getFirst();
                try {
                    int y = textComponent.modelToView(position.getOffset()).y;
                    y = (int) (height * ((float) y / totalHeight));
                    g.setColor(marker.getSecond());
                    g.fillRect(0, y, width, 2);
                } catch (BadLocationException ignored) {
                }
            }
        }

        @Override
        public Dimension preferredSize(final Dimension preferredSize) {
            preferredSize.width *= 1.5;
            preferredSize.height *= 1.5;
            return preferredSize;
        }
    }
}

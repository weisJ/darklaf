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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.util.Random;

import javax.swing.*;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import com.github.weisj.darklaf.listener.InsertDocumentListener;
import com.github.weisj.darklaf.properties.color.DarkColorModelHSB;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class ColoredTextDemo extends TextPaneDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ColoredTextDemo());
    }

    @Override
    protected JTextPane createTextComponent() {
        JTextPane textPane = super.createTextComponent();
        textPane.getDocument().addDocumentListener((InsertDocumentListener) () -> SwingUtilities.invokeLater(() -> {
            StyledDocument document = textPane.getStyledDocument();
            Random r = new Random();
            for (int i = 0; i < document.getLength() - 2; i++) {
                MutableAttributeSet a = new SimpleAttributeSet();
                Color c = DarkColorModelHSB.getColorFromHSBValues(r.nextDouble(), 1, 1);
                StyleConstants.setForeground(a, c);
                document.setCharacterAttributes(i, 1, a, true);
            }
        }));
        return textPane;
    }

    @Override
    public String getName() {
        return "Colored Text Demo";
    }
}

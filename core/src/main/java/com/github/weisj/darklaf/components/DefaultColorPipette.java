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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;

import javax.swing.*;

import com.github.weisj.darklaf.properties.icons.EmptyIcon;
import com.github.weisj.darklaf.ui.colorchooser.ColorListener;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.ui.util.TimerUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public class DefaultColorPipette extends ColorPipetteBase {
    private static final int SIZE = 36;
    private static final int DIALOG_SIZE = 50;
    private static final int MOUSE_OFF_X = 7;
    private static final int MOUSE_OFF_Y = -7;

    private final Rectangle captureRect = new Rectangle();
    private final Point previousLocation = new Point();
    private final Timer timer;
    protected final Color borderColor;
    private Graphics2D zoomGraphics;
    private BufferedImage zoomImage;

    public DefaultColorPipette(final JComponent parent, final ColorListener colorListener) {
        super(parent, colorListener);
        timer = TimerUtil.createNamedTimer("DefaultColorPipette", 5, e -> updatePipette());
        borderColor = UIManager.getColor("ColorChooser.pipetteBorderColor");
    }

    protected void updatePipette() {
        updatePipette(false);
    }

    @Override
    protected Color getPixelColor(final Point location) {
        return super.getPixelColor(getHotSPot(location));
    }

    @Override
    public Window show() {
        Window picker = super.show();
        timer.start();
        return picker;
    }

    @Override
    public void cancelPipette() {
        timer.stop();
        super.cancelPipette();
    }

    @Override
    protected Window getOrCreatePickerWindow() {
        Window pickerWindow = getPickerWindow();
        if (pickerWindow == null) {
            pickerWindow = super.getOrCreatePickerWindow();
            pickerWindow.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseExited(final MouseEvent event) {
                    updatePipette();
                }
            });
            pickerWindow.addMouseMotionListener(new MouseAdapter() {
                @Override
                public void mouseMoved(final MouseEvent e) {
                    updatePipette();
                }
            });
            pickerWindow.addFocusListener(new FocusAdapter() {
                @Override
                public void focusLost(final FocusEvent e) {
                    pickAndClose();
                }
            });

            pickerWindow.setSize(DIALOG_SIZE, DIALOG_SIZE);
            zoomImage = parent.getGraphicsConfiguration().createCompatibleImage(SIZE, SIZE, Transparency.TRANSLUCENT);

            zoomGraphics = (Graphics2D) zoomImage.getGraphics();
            zoomGraphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
        }

        return pickerWindow;
    }

    @Override
    protected JWindow createPickerWindow(final Window parent) {
        return new DefaultPickerWindow(this);
    }

    @Override
    protected Point adjustPickerLocation(final Point mouseLocation, final Window pickerWindow) {
        Point p = super.adjustPickerLocation(mouseLocation, pickerWindow);
        p.x += DIALOG_SIZE / 2 - MOUSE_OFF_X;
        p.y -= DIALOG_SIZE / 2 + MOUSE_OFF_Y;
        return p;
    }

    protected void updatePipette(final boolean force) {
        Window pickerWindow = getPickerWindow();
        if (pickerWindow != null && pickerWindow.isShowing()) {
            Point mouseLoc = updateLocation();
            if (mouseLoc == null) return;
            final Color c = getPixelColor(mouseLoc);
            if (!c.equals(getColor()) || !mouseLoc.equals(previousLocation) || force) {
                setColor(c);
                previousLocation.setLocation(mouseLoc);

                if (isKeyDown() && getPressedKeyCode() == KeyEvent.VK_SHIFT) {
                    Point p = pickerWindow.getLocationOnScreen();
                    p.y += pickerWindow.getHeight() - 2;
                    p.x += 2;
                    captureRect.setBounds(p.x - 9, p.y - 9, 18, 18);

                    BufferedImage capture = robot.createScreenCapture(captureRect);
                    zoomGraphics.drawImage(capture, 0, 0, zoomImage.getWidth(), zoomImage.getHeight(), this);
                }
                pickerWindow.repaint();
                notifyListener(c);
            }
        }
    }

    @Override
    public void dispose() {
        timer.stop();
        super.dispose();
        if (zoomGraphics != null) {
            zoomGraphics.dispose();
        }
        zoomImage = null;
    }

    protected Point getHotSPot(final Point location) {
        location.x -= MOUSE_OFF_X - 2;
        location.y -= MOUSE_OFF_Y + 2;
        return location;
    }

    @Override
    public boolean isAvailable() {
        if (robot != null) {
            robot.createScreenCapture(new Rectangle(0, 0, 1, 1));
            return true;
        }
        return false;
    }

    protected Icon getPipetteIcon() {
        Icon icon = UIManager.getIcon("ColorChooser.pipette.icon");
        if (icon == null) icon = DarkUIUtil.ICON_LOADER.getIcon("misc/pipette.svg", true);
        if (icon == null) icon = EmptyIcon.create(0);
        return icon;
    }

    protected Color getPipetteBorderColor() {
        return borderColor;
    }

    protected static class DefaultPickerWindow extends PickerWindow {

        private final DefaultColorPipette pipette;

        protected DefaultPickerWindow(final DefaultColorPipette pipette) {
            this.pipette = pipette;
        }

        @Override
        public void paint(final Graphics g2) {
            super.paint(g2);
            GraphicsUtil.setupStrokePainting(g2);
            Graphics2D g = (Graphics2D) g2;

            // Draw region to be recognised as inside the window.
            g.setColor(Color.WHITE);
            GraphicsContext config = GraphicsUtil.paintWithAlpha(g, 0.005f);
            Point p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, this);
            g.fillRect(p.x - 5, p.y - 5, 10, 10);
            config.restore();

            Icon icon = pipette.getPipetteIcon();
            if (pipette.isKeyDown() && pipette.getPressedKeyCode() == KeyEvent.VK_SHIFT) {
                Shape oldCLip = g.getClip();
                Ellipse2D.Float circ = new Ellipse2D.Float(icon.getIconWidth() - 4, 2,
                        getWidth() - icon.getIconWidth() - 2 + 4, getHeight() - 2 - icon.getIconHeight() + 4);
                g.setClip(circ);
                g.drawImage(pipette.zoomImage, icon.getIconWidth() - 4, 2, null);
                g.setClip(oldCLip);

                g.setColor(pipette.getPipetteBorderColor());
                g.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                g.draw(circ);
            }
            icon.paintIcon(null, g, 0, getHeight() - icon.getIconHeight());
        }
    }
}

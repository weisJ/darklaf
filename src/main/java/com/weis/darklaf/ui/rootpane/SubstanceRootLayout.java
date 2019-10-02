package com.weis.darklaf.ui.rootpane;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

class SubstanceRootLayout implements LayoutManager2 {
    public Dimension preferredLayoutSize(@NotNull final Container parent) {
        Dimension cpd, mbd, tpd;
        int cpWidth = 0;
        int cpHeight = 0;
        int mbWidth = 0;
        int mbHeight = 0;
        int tpWidth = 0;
        int tpHeight = 0;
        Insets i = parent.getInsets();
        JRootPane root = (JRootPane) parent;

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getPreferredSize();
        } else {
            cpd = root.getSize();
        }
        if (cpd != null) {
            cpWidth = cpd.width;
            cpHeight = cpd.height;
        }

        if (root.getJMenuBar() != null) {
            mbd = root.getJMenuBar().getPreferredSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }

        if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
            JComponent titlePane = ((DarkRootPaneUI) root.getUI()).getTitlePane();
            if (titlePane != null) {
                tpd = titlePane.getPreferredSize();
                if (tpd != null) {
                    tpWidth = tpd.width;
                    tpHeight = tpd.height;
                }
            }
        }

        return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth)
                                     + i.left + i.right,
                             cpHeight + mbHeight + tpHeight + i.top + i.bottom);
    }

    public Dimension minimumLayoutSize(@NotNull final Container parent) {
        Dimension cpd, mbd, tpd;
        int cpWidth = 0;
        int cpHeight = 0;
        int mbWidth = 0;
        int mbHeight = 0;
        int tpWidth = 0;
        int tpHeight = 0;
        Insets i = parent.getInsets();
        JRootPane root = (JRootPane) parent;

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getMinimumSize();
        } else {
            cpd = root.getSize();
        }
        if (cpd != null) {
            cpWidth = cpd.width;
            cpHeight = cpd.height;
        }

        if (root.getJMenuBar() != null) {
            mbd = root.getJMenuBar().getMinimumSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }
        if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
            JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                    .getTitlePane();
            if (titlePane != null) {
                tpd = titlePane.getMinimumSize();
                if (tpd != null) {
                    tpWidth = tpd.width;
                    tpHeight = tpd.height;
                }
            }
        }

        return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth)
                                     + i.left + i.right, cpHeight + mbHeight + tpHeight + i.top
                                     + i.bottom);
    }

    public Dimension maximumLayoutSize(@NotNull final Container target) {
        Dimension cpd, mbd, tpd;
        int cpWidth = Integer.MAX_VALUE;
        int cpHeight = Integer.MAX_VALUE;
        int mbWidth = Integer.MAX_VALUE;
        int mbHeight = Integer.MAX_VALUE;
        int tpWidth = Integer.MAX_VALUE;
        int tpHeight = Integer.MAX_VALUE;
        Insets i = target.getInsets();
        JRootPane root = (JRootPane) target;

        if (root.getContentPane() != null) {
            cpd = root.getContentPane().getMaximumSize();
            if (cpd != null) {
                cpWidth = cpd.width;
                cpHeight = cpd.height;
            }
        }

        if (root.getJMenuBar() != null) {
            mbd = root.getJMenuBar().getMaximumSize();
            if (mbd != null) {
                mbWidth = mbd.width;
                mbHeight = mbd.height;
            }
        }

        if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
            JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                    .getTitlePane();
            if (titlePane != null) {
                tpd = titlePane.getMaximumSize();
                if (tpd != null) {
                    tpWidth = tpd.width;
                    tpHeight = tpd.height;
                }
            }
        }

        int maxHeight = Math.max(Math.max(cpHeight, mbHeight), tpHeight);
        if (maxHeight != Integer.MAX_VALUE) {
            maxHeight = cpHeight + mbHeight + tpHeight + i.top + i.bottom;
        }

        int maxWidth = Math.max(Math.max(cpWidth, mbWidth), tpWidth);

        if (maxWidth != Integer.MAX_VALUE) {
            maxWidth += i.left + i.right;
        }

        return new Dimension(maxWidth, maxHeight);
    }

    public void layoutContainer(final Container parent) {
        JRootPane root = (JRootPane) parent;
        Rectangle b = root.getBounds();
        Insets i = root.getInsets();
        int nextY = 0;
        int w = b.width - i.right - i.left;
        int h = b.height - i.top - i.bottom;

        if (root.getLayeredPane() != null) {
            root.getLayeredPane().setBounds(i.left, i.top, w, h);
        }
        if (root.getGlassPane() != null) {
            root.getGlassPane().setBounds(i.left, i.top, w, h);
        }

        if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
            JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                    .getTitlePane();
            if (titlePane != null) {
                Dimension tpd = titlePane.getPreferredSize();
                if (tpd != null) {
                    int tpHeight = tpd.height;
                    titlePane.setBounds(0, 0, w, tpHeight);
                    nextY += tpHeight;
                }
            }
        }
        if (root.getJMenuBar() != null) {
            Dimension mbd = root.getJMenuBar().getPreferredSize();
            root.getJMenuBar().setBounds(0, nextY, w, mbd.height);
            nextY += mbd.height;
        }
        if (root.getContentPane() != null) {

            root.getContentPane().setBounds(0, nextY, w, h < nextY ? 0 : h - nextY);
        }
    }

    public void addLayoutComponent(final String name, final Component comp) {
    }

    public void removeLayoutComponent(final Component comp) {
    }

    public void addLayoutComponent(final Component comp, final Object constraints) {
    }

    public float getLayoutAlignmentX(final Container target) {
        return 0.0f;
    }

    public float getLayoutAlignmentY(final Container target) {
        return 0.0f;
    }

    public void invalidateLayout(final Container target) {
    }
}

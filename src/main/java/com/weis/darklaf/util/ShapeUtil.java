package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.List;

public final class ShapeUtil {

    @NotNull
    public static Path2D getRoundedPath2D(@NotNull final Polygon polygon, final int arcSize) {
        List<int[]> l = new ArrayList<>();
        for (int i = 0; i < polygon.npoints; i++) {
            l.add(new int[]{polygon.xpoints[i], polygon.ypoints[i]});
        }
        return getRoundedPath2D(l, arcSize);
    }

    @NotNull
    public static Path2D getRoundedPath2D(@NotNull final List<int[]> l, final int arcSize) {
        List<Point> list = new ArrayList<>();
        for (int[] point : l) {
            list.add(new Point(point[0], point[1]));
        }
        return getRoundedPath2DFromPoints(list, arcSize);
    }

    @NotNull
    public static Path2D getRoundedPath2DFromPoints(@NotNull final List<Point> l, final int arcSize) {
        l.add(l.get(0));
        l.add(l.get(1));
        Path2D p = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        p.moveTo(l.get(0).x, l.get(0).y);
        for (int pointIndex = 1; pointIndex < l.size() - 1; pointIndex++) {
            Point p1 = l.get(pointIndex - 1);
            Point p2 = l.get(pointIndex);
            Point p3 = l.get(pointIndex + 1);
            Point mPoint = calculatePoint(p1, p2, arcSize);
            p.lineTo(mPoint.x, mPoint.y);
            mPoint = calculatePoint(p3, p2, arcSize);
            p.curveTo(p2.x, p2.y, p2.x, p2.y, mPoint.x, mPoint.y);
        }
        return p;
    }

    @Contract("_, _, _ -> new")
    @NotNull
    private static Point calculatePoint(@NotNull final Point p1, @NotNull final Point p2, final double arcSize) {
        double d1 = Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2));
        double per = arcSize / d1;
        double d_x = (p1.x - p2.x) * per;
        double d_y = (p1.y - p2.y) * per;
        int xx = (int) (p2.x + d_x);
        int yy = (int) (p2.y + d_y);
        return new Point(xx, yy);
    }
}

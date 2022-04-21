/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import javafx.geometry.*;
import javafx.scene.transform.*;
import javax.annotation.*;

public class ArcEndpoint {
    ArcEndpoint(@Nonnull MNode.Port m, @Nonnull FGNode c) {
        meta = m;
        container = c;
        uname = container.uid+":"+meta.name;
    }
    @Nonnull final MNode.Port meta;
    @Nonnull final FGNode container;
    final String uname;
    private FGNode.PortView view;
    public void setView(FGNode.PortView v) { view = v; }
    public FGNode.PortView getView() {
        return view;
    }
    @Override public String toString() { return container.meta.name+"."+meta.name; }
    public Point2D getPosition(boolean right, Transform area) {
        try {
            var box = container.view.isExpanded() ? view : container.view;
            var lbl = box.getBoundsInLocal();
            var t = box.getLocalToSceneTransform();
            var ret = t.transform(lbl.getMinX(), lbl.getHeight() / 2);
            if(right) ret = ret.add(lbl.getWidth(), 0);
            return area.inverseTransform(ret);
        } catch(NonInvertibleTransformException ex) {
            Dlg.error("getPosition error", ex);
            return new Point2D(0,0);
        }
    }
}

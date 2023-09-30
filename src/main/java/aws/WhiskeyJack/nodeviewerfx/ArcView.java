/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.nodegraph.Arc;
import aws.WhiskeyJack.nodegraph.*;
import static aws.WhiskeyJack.nodegraph.ErrorCode.*;
import static aws.WhiskeyJack.nodeviewerfx.GraphView.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.util.*;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.shape.*;
import javafx.scene.transform.*;

public class ArcView extends Arc implements Selectable {
    private CubicCurve view;
    private Label tagView;

    @SuppressWarnings("unused")
    public ArcView(PortView a, PortView b) {
        super(a, b);
    }
    @Override
    public GraphView getContext() {
        return (GraphView) super.getContext();
    }
    @Override
    public Domain getDomain() {
        return null;
    }
    @Override
    public CubicCurve getView() {
        var r = view;
        if(r == null) {
            view = r = new CubicCurve();
            var controller = getContext();
            r.hoverProperty().addListener(b -> {
                if(view != null)
                    controller.getSelection().setHovered(view.isHover() ? this : null);
            });
            r.setOnDragOver(evt -> {
                if(DragAssist.createNode != null) {
                    evt.acceptTransferModes(TransferMode.ANY);
                    evt.consume();
                }
            });
            r.setOnDragDropped(evt -> {
                if(DragAssist.createNode != null) {
                    DragAssist.targetX = evt.getScreenX();
                    DragAssist.targetY = evt.getScreenY();
                    controller.getSelection().setHovered(this);
                    controller.make(DragAssist.createNode);
                }
                evt.setDropCompleted(true);
                evt.consume();
            });
            r.setOnDragEntered(evt -> {
                if(view != null)
                    view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, true);
            });
            r.setOnDragExited(evt -> {
                if(view != null)
                    view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, false);
            });
//            Tooltip.install(r, new Tooltip(getTag() != null ? getTag() : oneEnd().getName() + "->" + otherEnd().getName()));
            controller.getView().getChildren().add(r);
//            setTag(toString());
        }
        return r;
        // TODO Platform.runLater(() -> setViewText());
    }
    @Override
    public ArcView setMessage(ErrorCode ec, String m) {
        if(!Objects.equals(m, getMessage())) {
            super.setMessage(ec, m);
            Tooltip.install(getView(), m == null ? null : new Tooltip(m));
            var css = getView().getStyleClass();
            if(ec != allIsWell) css.add("error");
            else css.remove("error");
        }
        return this;
    }
    @Override
    public void delete() {
        super.delete();
        var c = getContext().getView().getChildren();
        if(view != null) {
            c.remove(view);
            view = null;
        }
        if(tagView != null) {
            c.remove(tagView);
            tagView = null;
        }
    }
    @Override
    public void setTag(String t) {
        if(!Objects.equals(t, getTag())) {
            super.setTag(t);
            if(isEmpty(t)) {
                if(tagView != null) {
                    getContext().getView().getChildren().remove(tagView);
                    tagView = null;
                }
            } else
                if(tagView == null) {
                    tagView = new Label(t);
                    tagView.getStyleClass().add("arctag");
                    tagView.layoutBoundsProperty().addListener((ov, was, is) -> {
                        tagView.setTranslateX(-is.getCenterX());
                        tagView.setTranslateY(-is.getCenterY());
                    });
                    getContext().getView().getChildren().add(tagView);
                } else tagView.setText(t);
            getContext().adjustArcs();
        }
    }
    public void reposition(Transform area) {
        var curve = getView();
        if(oneEnd() instanceof PortView a0 && otherEnd() instanceof PortView b0) {
            PortView a, b;
            if(b0.isOutputSide()) {
                a = b0;
                b = a0;
            } else {
                a = a0;
                b = b0;
            }
            var out = a.getPosition(area);
            var ox = out.getX();
            var oy = out.getY();
            var in = b.getPosition(area);
            var ix = in.getX();
            var iy = in.getY();
            var delta = ox <= ix ? Integer.min(100, ((int) (ix - ox)) / 3) : 100;
            curve.setStartX(ox);
            curve.setStartY(oy);
            var X1 = ox + (a.metadata.isOutputSide() ? delta : -delta);
            var Y1 = oy;
            curve.setControlX1(X1);
            curve.setControlY1(Y1);
            var X2 = ix + (b.metadata.isOutputSide() ? delta : -delta);
            var Y2 = iy;
            curve.setControlX2(X2);
            curve.setControlY2(Y2);
            curve.setEndX(ix);
            curve.setEndY(iy);
            if(tagView != null) {
                tagView.setLayoutX((X1 + X2) / 2);
                tagView.setLayoutY((Y1 + Y2) / 2);
            }
        }
    }
}

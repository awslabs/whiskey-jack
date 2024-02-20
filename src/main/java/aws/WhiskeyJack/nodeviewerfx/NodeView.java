/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.metadata.*;
import aws.WhiskeyJack.nodegraph.Node;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.util.*;
import java.util.function.*;
import javafx.geometry.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;
import javafx.scene.text.*;
import javax.annotation.*;

public class NodeView extends Node implements Selectable {
    public NodeView(@Nonnull GraphView parent, @Nonnull Node original) {
        super(parent, original.metadata);
        init();
        populateFrom(original);
    }
    @SuppressWarnings("LeakingThisInConstructor")
    public NodeView(@Nonnull GraphView parent, @Nonnull MetaNode mn) {
        super(parent, mn);
        init();
        try {
            pane.setUserData(this);
            parent.getView().getChildren().add(pane);
            parent.makeDraggable(this);
        } catch(Throwable t) {
            Dlg.error("Error adding node", t);
        }
    }
    public static NodeView of(Map contents, GraphView parent) {
        var mnode = NodeLibrary.singleton.createIfAbsent(get(contents, "metapath", "node/unknown"));
        var node = new NodeView(parent, mnode);
        node.populateFrom(contents);
        return node;
    }
    @Override
    public void setTag(String tag) {
        setName(tag);
    }
    @Override
    public void populateFrom(Map map) {
        super.populateFrom(map);
        pane.setLayoutX(get(map, "x", 0));
        pane.setLayoutY(get(map, "y", 0));
    }
    @Override
    public GraphView getContext() {
        return (GraphView) super.getContext();
    }

    private final VBox pane = new VBox();
    private boolean expanded = false;
    private final ImageView titleIcon = new ImageView(IconGoofy);
    private final GridPane contents = new GridPane();
    private final Text title = new Text("unknown");
    private final HBox titleRegion = new HBox(titleIcon, title);
    private void init() {
//        openClose.setFitHeight(12);
//        openClose.setPreserveRatio(true);
        HBox.setHgrow(contents, Priority.ALWAYS);
        HBox.setHgrow(titleRegion, Priority.ALWAYS);
        HBox.setHgrow(title, Priority.ALWAYS);
        contents.setHgap(10);
        contents.setPadding(noPadding);
        contents.getColumnConstraints().addAll(flushLeft, flushRight);
        contents.getStyleClass().add("nodeItem");
        pane.setFillWidth(true);
        pane.getStyleClass().add("fgpane");
        titleRegion.getStyleClass().add("fgtitle");
//        title.getStyleClass().add("fgtitletext");
//        openClose.getStyleClass().add("open");
        pane.getChildren().setAll(titleRegion);
        titleIcon.setFitHeight(16);
        titleIcon.setPreserveRatio(true);
        titleIcon.setOnMouseReleased(e -> setExpanded(!isExpanded()));
        establishIcon();
        setTitle(metadata.getName());
        setTooltip(metadata.getDescription());
        installPorts();
        setExpanded(true);
        establishDomain(null, getDomain());
        getContext().checkTypes();
    }
    @Override
    public VBox getView() {
        return pane;
    }
    private void installPorts() {
        ports.values().forEach(new Consumer<Port>() {
            int inrow = 0; // can't do this with a lambda!
            int outrow = 0;
            @Override
            public void accept(Port P) {
                var p = (PortView) P;
                var output = p.metadata.isOutputSide();
                contents.add(p.getView(),
                    output ? 1 : 0,
                    output ? outrow++ : inrow++);
            }
        });
    }
    @Override
    public void delete() {
        /* save arcs in a seperate list to avoid ConcurrentModificationException */
        final var t = new Object() {
            Port rout = null; // output port for replacement arc
            Port rin = null;
        };
        var arcs = new ArrayList<Arc>(5);
        ports.values().forEach(p0 -> {
            var p = (PortView) p0;
            p.forEachArc(a -> {
                arcs.add(a);
                if(p.isInputSide()) {
                    if(t.rout == null) t.rout = a.otherEnd(p);
                } else
                    if(t.rin == null) t.rin = a.otherEnd();
            });
        });
        arcs.forEach(a -> a.delete());
        getContext().getView().getChildren().remove(getView());
        getContext().remove(this);
        if(t.rout != null && t.rin != null) t.rout.connectTo(t.rin);
    }
    public void setTitle(String s) {
        title.setText(s);
    }
    private Tooltip tip;
    public void setTooltip(String tooltip) {
        if(!Utils.isEmpty(tooltip))
            if(tip == null) {
                tip = new Tooltip(tooltip);
                Tooltip.install(titleRegion, tip);
            } else
                tip.setText(tooltip);
        else if(tip != null) {
            Tooltip.uninstall(titleRegion, tip);
            tip = null;
        }
    }
    public final void setExpanded(boolean b) {
        if(b != expanded) {
            expanded = b;
//            openClose.getStyleClass().setAll(expanded ? "fgopen" : "fgclosed");
            pane.getChildren().remove(contents);
            if(expanded)
                pane.getChildren().add(contents);
//            titleIcon.setRotate(expanded ? 0 : 90);
            pane.resize(pane.getPrefWidth(), expanded ? pane.getPrefHeight() : 30);
            getContext().adjustArcs();
            establishIcon();
        }
    }
    private void establishIcon() {
        var icon = IconOK;
        if(!expanded) icon = IconHidden;
        else if(isInferred()) icon = IconRobot;
        titleIcon.setImage(icon);
    }

    @Override
    public Node setInferred(boolean b) {
        super.setInferred(b);
        establishIcon();
        var view = getView();
        if(view.getLayoutX()==0 || view.getLayoutY()==0) {
            var ctx = getContext();
            var bil = ctx.getView().getBoundsInLocal();
            view.setLayoutX(bil.getCenterX());
            view.setLayoutY(bil.getCenterY());
        }
        return this;
    }
    @Override
    public boolean canDrag() {
        return true;
    }
    public boolean isExpanded() {
        return expanded;
    }
    @Override
    public NodeView setDomain(Domain d) {
        var d0 = getDomain();
        super.setDomain(d);
        var d1 = getDomain();
        if(d0 != d1)
            establishDomain(d0, d1);
        return this;
    }
    @Override
    public Domain getMetaDomain() {
        return metadata.getDomain();
    }
    private void establishDomain(Domain d0, Domain d1) {
//        System.out.println("estDom " + this.getName() + " " + d0 + "->" + d1);
        getContext().changeDomain(this, d0, d1);
        var s = pane.getStyleClass();
        if(d0 != null)
            s.remove(d0.getStyleName());
        if(d1 != null)
            s.add(d1.getStyleName());
    }
    @Override
    protected void collectMore(Map map) {
        super.collectMore(map);
        putOpt(map, "x", pane.getLayoutX());
        putOpt(map, "y", pane.getLayoutY());
        putOpt(map, "expanded", isExpanded());
    }

//    private static final Image closeArrow = new Image(NodeView.class.getResourceAsStream("CloseArrow.png"));
    private static final Insets noPadding = new Insets(0, 0, 0, 0);
    private static final ColumnConstraints flushLeft = new ColumnConstraints();
    private static final ColumnConstraints flushRight = new ColumnConstraints();
    private static final Image IconBroken = new Image(NodeView.class.getResourceAsStream("1x/Broken.png"));
    private static final Image IconGoofy = new Image(NodeView.class.getResourceAsStream("1x/Goofy.png"));
    private static final Image IconHidden = new Image(NodeView.class.getResourceAsStream("1x/Hidden.png"));
    private static final Image IconOK = new Image(NodeView.class.getResourceAsStream("1x/OK.png"));
    private static final Image IconRobot = new Image(NodeView.class.getResourceAsStream("1x/Robot.png"));

    static {
        flushLeft.setHalignment(HPos.LEFT);
        flushLeft.setFillWidth(true);
        flushRight.setHalignment(HPos.RIGHT);
        flushRight.setFillWidth(true);
    }
}

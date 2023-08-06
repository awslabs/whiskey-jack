/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;
import javafx.scene.control.*;
import static javafx.scene.layout.Region.*;

public class DomainView implements Selectable {
//    private final GraphView context;
    private final Domain domain;
    private final Label view;
    private final Set<Node> nodes = new LinkedHashSet<>();
    @SuppressWarnings("LeakingThisInConstructor")
    DomainView(GraphView c, Domain d) {
//        context = c;
        domain = d;
        view = new Label(domain.toString());
        view.setMinSize(USE_PREF_SIZE, USE_PREF_SIZE);
        view.setMaxSize(USE_PREF_SIZE, USE_PREF_SIZE);
        view.getStyleClass().add("domainbox");
        view.setUserData(this);
        c.getView().getChildren().add(view);
        c.makeDraggable(this);
    }
    @Override
    public void delete() {
    }
    @Override
    public void setTag(String t) {
        view.setText(t);
    }
    @Override
    public Label getView() {
        return view;
    }
    public void reposition() {
        if(nodes.isEmpty())
            view.setVisible(false);
        else {
            view.setVisible(true);
            var minx = Double.MAX_VALUE;
            var miny = Double.MAX_VALUE;
            var maxx = Double.MIN_VALUE;
            var maxy = Double.MIN_VALUE;
            for(var n: nodes) {
                var v = ((NodeView) n).getView();
                var l = v.boundsInParentProperty().get();
                minx = Math.min(minx, l.getMinX());
                miny = Math.min(miny, l.getMinY() - 7);
                maxx = Math.max(maxx, l.getMaxX());
                maxy = Math.max(maxy, l.getMaxY());
            }
            view.setLayoutX(minx);
            view.setLayoutY(miny);
            view.setPrefSize(maxx - minx, maxy - miny);
        }
    }
    public void add(Node n) {
        System.out.println(domain + " add " + n.getName());
        nodes.add(n);
    }
    public void remove(Node n) {
        System.out.println(domain + " remove " + n.getName());
        nodes.remove(n);
    }
    @Override
    public boolean canDrag() {
        return nodes.stream().allMatch(n ->
                !(n instanceof Selectable s) || s.canDrag());
    }
    @Override
    public void endDrag() {
        nodes.forEach(n -> {
            if(n instanceof Selectable s) s.endDrag();
        });
    }
    @Override
    public void setDrag(double dx, double dy) {
        nodes.forEach(n -> {
            if(n instanceof Selectable s) s.setDrag(dx, dy);
        });
    }
}

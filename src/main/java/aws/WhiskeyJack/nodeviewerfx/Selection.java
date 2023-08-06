/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import java.util.*;
import java.util.function.*;

public class Selection {
    private final Set<Selectable> selectionSet = new HashSet<>();
    private Selectable hovered;
    public void clear() {
        selectionSet.forEach(s ->
                s.getView().getStyleClass().remove("selected"));
        selectionSet.clear();
    }
    public void add(Selectable s) {
        if(selectionSet.add(s))
            s.getView().getStyleClass().add("selected");
    }
    public void forEach(Consumer<Selectable> func) {
        if(selectionSet.isEmpty()) {
            if(getHovered() != null)
                func.accept(getHovered());
        } else selectionSet.forEach(func);
    }
    public Selectable getHovered() {
        return hovered;
    }
    public void setHovered(Selectable h) {
        hovered = h;
    }
    public boolean canDrag() {
        return selectionSet.stream().allMatch(s->s.canDrag());
    }
    public void onSelectionChanged() {
        System.out.println("on Selection Changed");
    }
}

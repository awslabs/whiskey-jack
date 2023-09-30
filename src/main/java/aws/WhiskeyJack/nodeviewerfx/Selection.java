/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public class Selection {
    private final ArrayList<Selectable> selectionSet = new ArrayList<>();
    private Selectable hovered;
    private Domain selectedDomain;
    public void clear() {
        selectionSet.forEach(s ->
                s.getView().getStyleClass().remove("selected"));
        selectionSet.clear();
        setSelectedDomain(Domain.any);
    }
    public void add(Selectable s) {
        if(s != null && !selectionSet.contains(s) && selectionSet.add(s)) {
            s.getView().getStyleClass().add("selected");
            if(selectionSet.size() == 1) setSelectedDomain(s.getDomain());
        }
    }
    public void forEach(Consumer<Selectable> func) {
        if(!selectionSet.isEmpty()) selectionSet.forEach(func);
        else if(getHovered() != null)
            func.accept(getHovered());
    }
    public Selectable getHovered() {
        return hovered;
    }
    public Selectable getFirstSelected() {
        return hovered != null || selectionSet.isEmpty() ? hovered : selectionSet.get(0);
    }
    public void setHovered(Selectable h) {
        hovered = h;
        if(selectionSet.isEmpty() && h != null)
            setSelectedDomain(h.getDomain());
    }
    public boolean canDrag() {
        return selectionSet.stream().allMatch(s -> s.canDrag());
    }
    public Domain getSelectedDomain() {
        return selectedDomain;
    }
    public void setSelectedDomain(Domain d) {
        if(d != null) {
            var prev = selectedDomain;
            selectedDomain = d;
            if(d != prev) domainListeners.forEach(l -> l.accept(d));
        }
    }

    private final CopyOnWriteArraySet<Consumer<Domain>> domainListeners = new CopyOnWriteArraySet<>();
    public void addDomainListener(Consumer<Domain> c) {
        domainListeners.add(c);
    }
    public void removeDomainListener(Consumer<Domain> c) {
        domainListeners.remove(c);
    }
    public void clearDomainListeners() {
        domainListeners.clear();
    }

    private final CopyOnWriteArraySet<Runnable> selectionListeners = new CopyOnWriteArraySet<>();
    public void addSelectionListener(Runnable c) {
        selectionListeners.add(c);
    }
    public void removeSelectionListener(Runnable c) {
        selectionListeners.remove(c);
    }
    public void clearSelectionListeners() {
        selectionListeners.clear();
    }
}

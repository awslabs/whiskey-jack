/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.util;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public class Topic {
    public static final long DEFAULT_VALUE_TIMESTAMP = 1;

    private Object value;
    protected final CopyOnWriteArraySet<Consumer<Topic>> watchers = new CopyOnWriteArraySet<>();


    /**
     * Subscribe to a topic and invoke the subscriber right away on the same thread for a new subscriber.
     * <p>
     * This is the preferred way to get a value from a configuration. Instead of {@code setValue(configValue.getOnce())}
     * use {@code configValue.get((nv,ov)->setValue(nv)) }
     * This way, every change to the config file will get forwarded to the object.
     *</p>
     *
     * @param s subscriber
     * @return this topic
     */
    public Topic subscribe(Consumer<Topic> s) {
        if (addWatcher(s)) {
            // invoke the new subscriber right away
            s.accept(this);
        }
        return this;
    }

    /**
     * This should rarely be used.Instead, use subscribe(Subscriber). Not synchronized with setState(). The returned value is the value of the last completed setState().
     * @return value
     */
    public Object getOnce() {
        return value;
    }
    
    /**
     * Add a watcher.
     *
     * @param s a watcher to be added
     * @return true if this is a new watcher; false if its a duplicate
     */
    protected boolean addWatcher(Consumer<Topic> s) {
        if (s != null) {
            return watchers.add(s);
        }
        return false;
    }

    /**
     * Remove a subscriber to stop being called for updates.
     *
     * @param s subscriber to remove
     */
    public void remove(Consumer<Topic> s) {
        watchers.remove(s);
    }

    public Topic withValue(Object nv) {
        if(!Objects.equals(value,nv)) {
            value = nv;
            fire();
        }
        return this;
    }
    protected void fire() {
        for (var s : watchers) {
            s.accept(this);
        }

    }

}

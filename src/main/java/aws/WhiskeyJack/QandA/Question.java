/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.QandA;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.util.*;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.module.*;
import com.fasterxml.jackson.databind.ser.std.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public class Question implements Comparable<Question> {
    public int priority;
    public boolean requested;
    public Object value;
    public Glob nodeMatch;
    final Map<String, Object> fields;
    private Set<Consumer<Question>> listeners;

    Question(Map m) {
        fields = m;
        value = get("default", "");
        priority = get("priority", 50);
        nodeMatch = Glob.compile(get("node","*"));
    }
    @Override
    public int compareTo(Question o) {
        var delta = o.priority-priority;
        return delta!=0 ? delta : get("label","zzz").compareTo(o.get("label", "zzz"));
    }
    public final Object get(String field, Object dflt) {
        return fields.getOrDefault(field, dflt);
    }
    public final boolean get(String field, boolean dflt) {
        return Coerce.toBoolean(fields.getOrDefault(field, dflt));
    }
    public final String get(String field, String dflt) {
        return Coerce.toString(fields.getOrDefault(field, dflt));
    }
    public final int get(String field, int dflt) {
        return Coerce.toInt(fields.getOrDefault(field, dflt));
    }
    public static List<Question> extract(Predicate<Question> pred) {
        return QuestionsByTag.byTag.values().stream().filter(pred).sorted().toList();
    }
    public static Question question(String tag) {
        return QuestionsByTag.get(tag);
    }
    public synchronized void listen(Consumer<Question> listener) {
        if(listeners==null) listeners = new CopyOnWriteArraySet<>();
        listeners.add(listener);
    }
    public boolean isTrue() {
//        System.out.println(get("tag","?")+" "+get("label","unlabelled")+" = "+value);
        return Coerce.toBoolean(value);
    }
    public static void listen(String tag, Consumer<Question> listener) {
        question(tag).listen(listener);
    }
    public void clearListeners() {
        listeners = null;
    }
    public synchronized void remove(Consumer<Question> listener) {
        if(listeners!=null) {
            listeners.remove(listener);
            if(listeners.isEmpty())
                listeners = null;
        }
    }
    public void fire(Object nv) {
        value = nv;
        if(listeners!=null)
            for(var l:listeners) l.accept(this);
    }
    
//<editor-fold defaultstate="collapsed" desc="serialization">
    public static class ItemSerializer extends StdSerializer<Question> {
        public ItemSerializer() {
            this(null);
        }
        public ItemSerializer(Class<Question> t) {
            super(t);
        }
        @Override
        public void serialize(
                Question value, JsonGenerator jgen, SerializerProvider provider)
                throws IOException, JsonProcessingException {
            jgen.writeStartObject();
            for(var kv: value.fields.entrySet())
                if(!"tag".equals(kv.getKey()))
                    jgen.writeObjectField(kv.getKey(), kv.getValue());
            jgen.writeEndObject();
        }
    }
    static {
        var module = new SimpleModule();
        module.addSerializer(Question.class, new ItemSerializer());
        QuestionsByTag.fileio.registerModule(module);
    }
//</editor-fold>
}
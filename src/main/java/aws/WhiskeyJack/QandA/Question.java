/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.QandA;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.ser.std.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public class Question implements Comparable<Question> {
    private final int priority;
    private Type type;
//    private public boolean requested;
    private Object value = "<uninit>";
    private Glob nodeMatch;
    private Domain domain;
    private final Map<String, Object> fields;
    private Set<Consumer<Question>> listeners;

    Question(Map m) {
        fields = m;
        setValue(get("default", "<missing>"));
        priority = get("priority", 50);
        nodeMatch = Glob.compile(get("node", "*"));
        domain = Domain.of(get("domain","any"), Domain.any);
    }
    public Question(String tag) {
        fields = Map.of("label", "Placeholder for "+tag);
        priority = 10;
        nodeMatch = Glob.compile(get("node", "*"));
        domain = Domain.any;
        type = Type.bool;
    }
    @Override
    public int compareTo(Question o) {
        var delta = o.priority - priority;
        return delta != 0 ? delta : get("label", "zzz").compareTo(o.get("label", "zzz"));
    }
    @Override public String toString() {
        return get("tag","<unknown>")+":"+getValue();
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
    public static Question question(String tag, Domain d) {
        return QuestionsByTag.get(tag, d);
    }
    public synchronized void listen(Consumer<Question> listener) {
        if(listeners == null) listeners = new CopyOnWriteArraySet<>();
        listeners.add(listener);
    }
    public boolean isTrue() {
        return Coerce.toBoolean(getValue());
    }
    public String asString() {
        return Coerce.toString(getValue());
    }
    public int asInt() {
        return Coerce.toInt(getValue());
    }
    public double asDouble() {
        return Coerce.toDouble(getValue());
    }
    public Type getType() { 
        var t = type;
        if(t==null)            
            type = t = Type.of(get("type", "boolean"), Type.err);
        return t;
    }
    public static void listen(String tag, Consumer<Question> listener) {
        question(tag).listen(listener);
    }
    public void clearListeners() {
        listeners = null;
    }
    public synchronized void remove(Consumer<Question> listener) {
        if(listeners != null) {
            listeners.remove(listener);
            if(listeners.isEmpty())
                listeners = null;
        }
    }
    public void fire() {
        if(listeners != null)
            for(var l: listeners)
                l.accept(this);
    }
    public final Object getValue() {
        return value;
    }
    public final void setValue(Object v) {
        var nv = getType().coerce(v, value);
        if(!Objects.equals(nv, value)) {
            value = nv;
            fire();
        }
    }
    static {
        DataIO.yaml.addSerializer(Question.class, new StdSerializer<Question>(Question.class) {
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
        });
    }
    public Glob getNodeMatch() {
        return nodeMatch;
    }
    public void setNodeMatch(Glob nodeMatch) {
        this.nodeMatch = nodeMatch;
    }
    public Domain getDomain() {
        return domain;
    }
    public void setDomain(Domain domain) {
        this.domain = domain;
    }
}

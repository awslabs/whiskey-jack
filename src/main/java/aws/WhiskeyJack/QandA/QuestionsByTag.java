/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.QandA;

import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Exec.*;
import static aws.WhiskeyJack.util.Utils.*;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.dataformat.yaml.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

public class QuestionsByTag {
    static final Map<String, Question> byTag = new ConcurrentHashMap<>();
    public static Question get(String tag) {
        return byTag.computeIfAbsent(tag, t -> {
            log("Q&A missing tag " + t);
            return new Question(Map.of("tag", t,
                    "label", "Missing " + t));
        });
    }
    private static void log(String msg) {
        System.out.println(msg);
    }
    public static boolean loadFile(URL p) {
        if(p == null) return false;
        var v = YAMLio.read(p);
        if(v==null) return false;
        add(v,null);
        return true;
    }
    public static void add(Object o, Object tag) {
//        System.out.println("add "+tag+"  "+deepToString(o)+"\n\t"+o.getClass());
        if(o.getClass().isArray()) {
            var len = Array.getLength(o);
            for(int i = 0; i < len; i++) {
                System.out.println("Slot "+i);
                add(Array.get(o, i), "");
            }
        } else if(o instanceof List l)
            for(var v:l) add(v, "");
        else if(o instanceof Map m)
            if(tag == null)
                m.forEach((k, v) -> add(v, k.toString()));
            else {
                if(!isEmpty(tag))
                    m.put("tag", tag);
                else tag = m.get("tag");
                var l = m.get("label");
                if(!(tag instanceof String st))
                    log("Entry missing tag " + deepToString(m));
                else if(!(l instanceof String))
                    log("Entry missing label " + deepToString(m));
                else if(byTag.containsKey(st))
                    log("Duplicate tag " + deepToString(m));
                else byTag.put(st, new Question(m));
            }
        else 
        System.out.println("Unexpected Q "+tag+"  "+deepToString(o));
    }
    public static void dump() {
        YAMLio.write(byTag, deTilde("~/questions.yaml"));
    }
    static {
        loadFile(QuestionsByTag.class.getResource("/questions-en.yml"));
    }
}

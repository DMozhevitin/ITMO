package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public class MessageServlet extends HttpServlet {
    private List<Message> messages = new ArrayList<>();

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws  IOException {
        String uri = request.getRequestURI();
        PrintWriter writer = new PrintWriter(response.getOutputStream(), false, StandardCharsets.UTF_8);
//        response.setCharacterEncoding("windows-1251"); // ебало обоссу за такое
        if (uri.endsWith("/message/auth")) {
            String userName = parseRequestBody(request, "user");
            request.getSession().setAttribute("user", userName);
            respondWithJson(response, userName, writer);
        } else if (uri.endsWith("/message/add")) {
            String userName = String.valueOf(request.getSession().getAttribute("user"));
            String text = parseRequestBody(request, "text");
            messages.add(new Message(userName, text));
        } else if (uri.endsWith("message/findAll")) {
            respondWithJson(response, messages, writer);
        } else {
            response.setStatus(404);
        }
    }

    private void respondWithJson(HttpServletResponse response, Object content, PrintWriter writer) throws IOException {
        response.setContentType("application/json");
        writer.print(new Gson().toJson(content));
        writer.flush();
        writer.close();
    }

    private String readRequestBody(HttpServletRequest request) throws IOException {
        BufferedReader reader = request.getReader();
        StringBuilder s = new StringBuilder();
        while (reader.ready()) {
            s.append(reader.readLine());
        }
        System.out.println(s.toString());
        reader.close();
        return URLDecoder.decode(s.toString(), StandardCharsets.UTF_8);
    }

    private String parseRequestBody(HttpServletRequest request, String attributeName) throws IOException {
        String body = readRequestBody(request);
        String res = "";
        if (body.contains(attributeName + "=")) {
            res = body.substring(body.indexOf('=') + 1);
        }

        return res;
    }

    private static class Message {
        private String user;
        private String text;

        private Message(String user, String text) {
            this.user = user;
            this.text = text;
        }

        public String getUser() {
            return user;
        }

        public String getText() {
            return text;
        }
    }
}

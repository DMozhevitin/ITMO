//package ru.itmo.wp.servlet;
//
//import ru.itmo.wp.util.ImageUtils;
//
//import javax.servlet.ServletException;
//import javax.servlet.http.HttpServlet;
//import javax.servlet.http.HttpServletRequest;
//import javax.servlet.http.HttpServletResponse;
//import java.io.*;
//import java.nio.file.Files;
//import java.util.Base64;
//
//public class PictureServlet extends HttpServlet {
//    @Override
//    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
//        resp.setContentType("text/html");
//        byte[] b = ImageUtils.toPng(String.valueOf(228));
//        byte[] base64 = Base64.getEncoder().encode(b);
//        System.out.println(new String(base64));
//
//        resp.setContentType("text/html");
//
//        StringBuilder s = new StringBuilder();
//        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("C:\\web\\lab3\\taskA\\src\\main\\webapp\\static\\captcha.html")));
//        while (reader.ready()) {
//            s.append(reader.readLine());
//        }
//
//        resp.getWriter().write("<head> <link rel=\"shortcut icon\" href=\"\"> </head> <body> <img src=\"data:image/png;base64," + new String(base64) + "\"/> <body>");
//        resp.getWriter().write(s.toString());
//        resp.getWriter().flush();
//    }
//}

package ru.itmo.wp.servlet;

import ru.itmo.wp.util.ImageUtils;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpFilter;
import javax.servlet.http.HttpServletRequest;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Base64;
import java.util.Random;

public class CaptchaFilter extends HttpFilter {
    private static Random rng = new Random();

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest servletRequest = (HttpServletRequest) request;
        if ("GET".equals(servletRequest.getMethod())) {
            Integer expect = (Integer) servletRequest.getSession().getAttribute("Expect");

            if (expect != null && servletRequest.getParameter("number") != null) {
                String number = servletRequest.getParameter("number");
                if (number.equals(String.valueOf(expect))) {
                    servletRequest.getSession().removeAttribute("Expect");
                    servletRequest.getSession().setAttribute("Valid-Captcha", true);
                    chain.doFilter(request, response);
                } else {
                    expect = respondWithCaptcha(servletRequest, response);
                    servletRequest.getSession().setAttribute("Expect", expect);
                    response.getWriter().flush();
                }
            } else if (servletRequest.getSession().getAttribute("Valid-Captcha") == null) {
                int expected = respondWithCaptcha(servletRequest, response);
                servletRequest.getSession().setAttribute("Expect", expected);
                response.getWriter().flush();
            } else {
                chain.doFilter(request, response);
            }

        }
    }

    private int respondWithCaptcha(HttpServletRequest request, ServletResponse response) throws IOException {
        int number = rng.nextInt(1000);
        byte[] b = ImageUtils.toPng(String.valueOf(number));
        byte[] base64 = Base64.getEncoder().encode(b);

        response.setContentType("text/html");

        StringBuilder s = new StringBuilder();
        BufferedReader reader = new BufferedReader(new InputStreamReader(
                new FileInputStream(System.getenv("servletRootPath") + "/main/webapp/static/captcha.html")));
        while (reader.ready()) {
            s.append(reader.readLine());
        }

        String html = s.toString();
        html = html.replaceAll("FORM_URL", request.getRequestURI());
        html = html.replaceAll("IMG_DATA", new String(base64));
        response.getWriter().write(html);

        return number;
    }
}

package ru.itmo.tpl.util;

import ru.itmo.tpl.model.Color;
import ru.itmo.tpl.model.Post;
import ru.itmo.tpl.model.User;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirayanov", "Mikhail Mirzayanov", Color.BLACK),
            new User(2, "tourist", "Genady Korotkevich", Color.RED),
            new User(3, "emusk", "Elon Musk", Color.GREEN),
            new User(5, "pashka", "Pavel Mavrin", Color.RED),
            new User(7, "geranazavr555", "Georgiy Nazarov", Color.GRAY),
            new User(11, "cannon147", "Erofey Bashunov", Color.GREEN)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "Codeforces Round #593 (Div. 2)", "Hello, Codeforces!\n" +
                    "\n" +
                    "I'd like to invite you to take part in Codeforces Round #593 (Div. 2). It will held on четверг, 17 октября 2019 г. в 16:35. The round will be rated for the participants with rating lower than 2100.\n" +
                    "\n" +
                    "You will be given 6 problems and 2 hours to solve them. The score distribution will be announced later.\n" +
                    "\n" +
                    "The problems of this round were developed by me. Thanks a lot to isaf27 for his excellent coordination, to Mike MikeMirzayanov Mirzayanov for Codeforces and Polygon platform, and to marX, antontrygubO_o, NIWIS, win11905 for testing.\n" +
                    "\n" +
                    "This is my first round ever. I hope everyone will enjoy it.\n" +
                    "\n" +
                    "Good luck!", 1),
            new Post(2, "Codeforces Round #592 (Div. 2)", "Привет, Codeforces!\n" +
                    "\n" +
                    "13 октября 2019 года в 12:05 MSK состоится Codeforces Round #592 (Div. 2). Обратите внимание на необычное время начала раунда!\n" +
                    "\n" +
                    "Раунд будет рейтинговым для участников второго дивизиона (с рейтингом менее 2100). Условия будут доступны как на русском, так и на английском языках.\n" +
                    "\n" +
                    "Этот раунд проводится по задачам регионального этапа Всероссийской командной олимпиады школьников по программированию 2019, проходящего в Саратове. Задачи вместе со мной придумывали и готовили Иван BledDest Андросов и Владимир Vovuh Петров.", 2),
            new Post(3, "Educational Codeforces Round 74", "Привет, Codeforces!\n" +
                    "\n" +
                    "В вторник, 8 октября 2019 г. в 17:35 состоится Educational Codeforces Round 74 (рейтинговый для Див. 2).\n" +
                    "\n" +
                    "Продолжается серия образовательных раундов в рамках инициативы Harbour.Space University! Подробности о сотрудничестве Harbour.Space University и Codeforces можно прочитать в посте.\n" +
                    "\n" +
                    "Этот раунд будет рейтинговым для участников с рейтингом менее 2100. Соревнование будет проводиться по немного расширенным правилам ICPC. Штраф за каждую неверную посылку до посылки, являющейся полным решением, равен 10 минутам. После окончания раунда будет период времени длительностью в 12 часов, в течение которого вы можете попробовать взломать абсолютно любое решение (в том числе свое). Причем исходный код будет предоставлен не только для чтения, но и для копирования.", 3)
    );

    private static void setPostsCnt(User user) {
        long cnt = 0;
        for (Post post : getPosts()) {
            if (user.getId() == post.getUser_id()) {
                cnt++;
            }

            user.setPostCnt(cnt);
        }
    }

    private static List<User> getUsers() {
        return USERS;
    }

    private static List<Post> getPosts() {
        return POSTS;
    }

    public static void putData(Map<String, Object> data) {
        data.put("users", getUsers());
        data.put("posts", getPosts());

        for (User user : getUsers()) {
            if (data.get("logged_user_id") != null && user.getId() == (long) (data.get("logged_user_id"))) {
                data.put("user", user);
            }
            setPostsCnt(user);
        }

    }

}

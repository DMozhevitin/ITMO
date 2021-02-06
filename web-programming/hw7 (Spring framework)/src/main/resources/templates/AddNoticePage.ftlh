<#import "commons.ftlh" as c>
<#import "/spring.ftl" as spring>
<#-- @ftlvariable name="noticeForm" type="ru.itmo.wp.form.NoticeForm" -->

<@c.page>
    <div class="notice form-box">
        <div class="header">Create Notice</div>
        <form method="post" action="">
            <input type="hidden" name="action" value="addNotice"/>
            <div class="field">
                <div class="name">
                    <label for="noticeContent">Text</label>
                </div>
                <div class="value">
                    <label>
                        <textarea name="noticeContent"></textarea>
                    </label>
                </div>
                <@c.error "noticeForm.noticeContent"/>
            </div>

            <div class="button-field">
                <input type="submit" value="Create">
            </div>
        </form>
    </div>
</@c.page>
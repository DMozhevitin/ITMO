<template>
    <div class="comment">
        <div class="comment-header">
            <span>{{commentWriter.login}}</span>
        </div>
        <span>{{comment.text}}</span>
    </div>
</template>

<script>
    export default {
        name: "Comment",
        props: ['comment', 'users'],
        computed: {
            commentWriter: function() {
                return Object.values(this.users).filter(u => u.id === this.comment.userId)[0];
            }
        }
    }
</script>

<style scoped>
    .comment {
        padding: 0;
        border: 1px solid var(--border-color);
        border-radius: var(--border-radius);
        margin-bottom: 1.5rem;
    }

    .comment span {
        padding-left: 0.25rem;
        height: 1.5rem;
    }

    .comment-header span {
        padding-left: 0.25rem;
    }

    .comment-header {
        padding-bottom: 0.5rem;
        font-weight: bolder;
        height: 0.8rem;
        border-bottom: 1px solid var(--border-color);
    }
</style>

<template>
    <div class="full-post">
        <MiddlePost :post="post" :users="users" :comments="comments"/>

        <h3 v-if="postComments.length">Comments: </h3>
        <Comment v-for="comment in postComments" :key="comment.id"
                 :comment="comment" :users="users"></Comment>
    </div>
</template>

<script>
    import MiddlePost from "./MiddlePost";
    import Comment from "../common/Comment";

    export default {
        name: "FullPost",
        components: {Comment, MiddlePost},
        props: ['post', 'users', 'comments'],
        computed: {
            postComments: function() {
                return Object.values(this.comments).filter(c => c.postId === this.post.id)
            }
        }
    }
</script>

<style scoped>
    h3 {
        color: #3B5998;
    }
</style>

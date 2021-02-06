<template>
    <div class="full-post">
        <MiddlePost :post="post"/>

        <form v-if="user" class="comment-form" @submit.prevent="onAddComment">
            <div>
                <h3>Add comment</h3>
                <textarea id="text" rows="20" v-model="commentText"></textarea>
            </div>
            <div class="error">{{error}}</div>
            <div>
                <input :disabled="addingComment" type="submit" value="Add"/>
            </div>

        </form>

        <div v-if="renderComments" class="comments">
            <h3 v-if="comments.length">Comments: </h3>
            <Comment v-for="comment in comments" :key="comment.id"
                     :comment="comment"></Comment>
        </div>
    </div>
</template>

<script>
    import MiddlePost from "./MiddlePost";
    import Comment from "../common/Comment";
    import axios from 'axios'

    export default {
        name: "FullPost",
        components: {
            MiddlePost,
            Comment
        },
        props: ['post', 'user'],
        data: function () {
            return {
                renderComments: true,
                addingComment: false,
                commentText: "",
                comments: []
            }
        },
        methods: {
            onAddComment: function () {
                this.addingComment = true;

                axios.post("/api/1/post/" + this.post.id, {
                    text: this.commentText,
                }).then(
                    this.getComments
                ).then(this.hideComments).then(this.showComments);

                this.addingComment = false;
                this.commentText = "";
            },
            hideComments() {
                this.renderComments = false;
            },
            showComments() {
                this.$nextTick(() => {
                    this.renderComments = true;
                });
            },
            getComments() {
                axios.get("/api/1/comments/" + this.post.id).then(
                    response => {
                        this.comments = response.data;
                    }
                );
            }
        },
        beforeMount() {
            this.getComments();
        }
    }
</script>

<style scoped>
    h3 {
        color: #3B5998;
    }

    .comment-form textarea {
        width: 15rem;
        height: 2rem;
    }
</style>

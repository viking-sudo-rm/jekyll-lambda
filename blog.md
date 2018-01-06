---
layout: default
title: Blog
permalink: /blog/
comments: false
author_footer: false
---

<div>

  	{% assign index = true %}

    {% for post in site.posts %}
		{% assign page = post %}
		{% include post.html %}
    {% endfor %}

  <p class="rss-subscribe">Subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a> or browse <a href="https://github.com/{{ site.github_site_repo }}" target="_blank">source on GitHub</a>.</p>

</div>
/*
 * webmention.js -- Fetch and display mentions from webmention.io.
 * Copyright © 2019 - 2020 Jakob L. Kreuze <zerodaysfordays@sdf.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */

function buildApiUri() {
  const ref = "jakob.space";
  const slug = /[^\/]*(?=\.html)/.exec(window.location.href);
  const endpoints = [
    `http://${ref}/${slug}.html`,
    `https://${ref}/${slug}.html`,
    `http://${ref}/blog/${slug}.html`,
    `https://${ref}/blog/${slug}.html`,
  ];
  return ["https://webmention.io/api/mentions.jf2?"].concat(
    endpoints.map((endpoint) => {
      return `target[]=${endpoint}`;
    }).join("&")
  ).join("");
}

function getData(url, callback) {
  if (fetch) {
    fetch(url).then(function(response) {
      if (response.status >= 200 && response.status < 300) {
        return Promise.resolve(response);
      } else {
        return Promise.reject(new Error(`Request failed: ${response.statusText}`));
      }
    }).then(function(response) {
      return response.json();
    }).then(callback);
  } else {
    let xhr = new XMLHttpRequest();
    xhr.onload = function(data) {
      callback(JSON.parse(data));
    }
    xhr.onerror = function(error) {
      throw new Error(`Request failed: ${error}`);
    }
  }
}

function makeComment(comment) {
  const strip = (uri) => {
    const sep = "://";
    return uri.substring(uri.indexOf(sep) + sep.length);
  };

  const element = (type, attributes) => {
    let res = document.createElement(type);
    for (const attribute in attributes) {
      res.setAttribute(attribute, attributes[attribute]);
    }
    return res;
  };

  // Create the h-card section.
  let avatar = element("img", {
    "class": "u-photo",
    "src": comment.url.startsWith("https://lobste.rs/")
      ? "/static/image/lobsters.png"
      : comment.author.photo || "/static/image/default-icon.png"
  });

  let authorName = element("a", {
    "class": "p-name u-url",
    "href": comment.author.url
  });
  authorName.textContent = comment.author.name;

  let authorURI = element("a", {
    "class": "author_url",
    "href": comment.author.url
  });
  authorURI.textContent = strip(comment.author.url);

  let hCardContainer = element("div", {
    "class": "p-author h-card author"
  });
  hCardContainer.appendChild(avatar);
  hCardContainer.appendChild(authorName);
  hCardContainer.appendChild(authorURI);

  // Create the content section.
  let contentContainer = element("div", {
    "class": "e-content p-name comment-content",
  });

  if (comment.hasOwnProperty("content")) {
    contentContainer.textContent = comment.content.text;
  } else {
    let em = element("em", {});
    if (comment.hasOwnProperty("wm-property")) {
      if ("repost-of" === comment["wm-property"]) {
        em.textContent = "Reposted this!";
      } else if ("like-of" === comment["wm-property"]) {
        em.textContent = "Favorited this!";
      } else {
        em.textContent = "No text provided :(";
      }
    } else {
      em.textContent = "No text provided :(";
    }
    contentContainer.appendChild(em);
  }

  // Create the metaline section.
  let pubTime = comment.published || comment["wm-received"];
  let time = element("time", {
    "class": "dt-published",
    "datetime": pubTime,
  });
  time.textContent = (new Date(pubTime)).toString();

  let linkBack = element("a", {
    "class": "u-url",
    "href": comment.url
  });
  linkBack.appendChild(time);

  let metalineContainer = element("div", {
    "class": "metaline"
  });
  metalineContainer.appendChild(linkBack);

  // Put it all together.
  let wrapper = element("li", {
    "class": "p-comment h-cite comment",
  });
  wrapper.appendChild(hCardContainer);
  wrapper.appendChild(contentContainer);
  wrapper.appendChild(metalineContainer);

  return wrapper;
}

getData(buildApiUri(), (json) => {
  json.children
    .map(makeComment)
    .forEach((elem) => {
      document.getElementById("webmention-container").appendChild(elem);
    })});

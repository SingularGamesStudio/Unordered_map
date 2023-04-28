#pragma once

#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <type_traits>
#include <unordered_map>

template <typename T, typename Allocator = std::allocator<T>>
class List {
    class BaseNode {
       public:
        BaseNode* next;
        BaseNode* prev;

        BaseNode() : next(this), prev(this) {}

        BaseNode(const BaseNode& other) : next(other.next), prev(other.prev) {}
        BaseNode& operator=(const BaseNode& other) {
            if (&other == this) return *this;
            next = other.next;
            prev = other.prev;
            return *this;
        }

        ~BaseNode() {
            prev->next = next;
            next->prev = prev;
        }
    };
    class Node : public BaseNode {
       public:
        T value;
    };
    Allocator alloc;
    using nodeAllocType =
        typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;

    BaseNode end_node;
    size_t sz;
    nodeAllocType node_alloc;

    Node* allocNode() {
        return std::allocator_traits<nodeAllocType>::allocate(node_alloc, 1);
    }

    void deallocNode(Node* node) {
        std::allocator_traits<nodeAllocType>::deallocate(node_alloc, node, 1);
    }

    void push_back_empty() {
        Node* node = allocNode();
        try {
            std::allocator_traits<Allocator>::construct(alloc, &(node->value));
        } catch (...) {
            deallocNode(node);
            throw;
        }
        node->prev = end_node.prev;
        node->next = &end_node;
        node->next->prev = node;
        node->prev->next = node;
        sz++;
    }

   public:
    template <typename T1>
    class BaseIterator {
        friend class List;

       private:
        List::BaseNode* node;
        BaseIterator(List::BaseNode* ptr) : node(ptr) {}

       public:
        using difference_type = int;
        using value_type = T1;
        using reference = T1&;
        using pointer = T1*;
        using iterator_category = std::bidirectional_iterator_tag;

        BaseIterator& operator++() {
            node = node->next;
            return *this;
        }

        BaseIterator operator++(int) {
            BaseIterator copy = *this;
            ++*this;
            return copy;
        }
        BaseIterator& operator--() {
            node = node->prev;
            return *this;
        }

        BaseIterator operator--(int) {
            BaseIterator copy = *this;
            --*this;
            return copy;
        }

        bool operator==(const BaseIterator& other) const = default;

        T1& operator*() {
            return *reinterpret_cast<T1*>(&(static_cast<Node*>(node)->value));
        }
        T1* operator->() {
            return reinterpret_cast<T1*>(&(static_cast<Node*>(node)->value));
        }

        operator BaseIterator<const T1>() const {
            BaseIterator<const T1> res(node);
            return res;
        }
    };

    void clear() {
        while (sz > 0) {
            pop_back();
        }
    }

    List(Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {}
    List(size_t n, const T& value, Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {
        try {
            for (size_t i = 0; i < n; i++) {
                push_back(value);
            }
        } catch (...) {
            clear();
            throw;
        }
    }
    List(size_t n, Allocator alloc)
        : alloc(alloc), end_node(BaseNode()), sz(0), node_alloc(alloc) {
        try {
            for (size_t i = 0; i < n; i++) {
                push_back_empty();
            }
        } catch (...) {
            clear();
            throw;
        }
    }
    // here clang-tidy does not see initialization of sz
    // NOLINTNEXTLINE
    List() : List(Allocator()) {}
    // NOLINTNEXTLINE
    List(size_t n, const T& value) : List(n, value, Allocator()) {}
    // NOLINTNEXTLINE
    List(size_t n) : List(n, Allocator()) {}

    List(const List& other)
        : alloc(std::allocator_traits<
                Allocator>::select_on_container_copy_construction(other.alloc)),
          end_node(BaseNode()),
          sz(0),
          node_alloc(alloc) {
        if (&other == this) return;
        try {
            for (const_iterator it = other.begin(); it != other.end(); it++) {
                push_back(*it);
            }
        } catch (...) {
            clear();
            throw;
        }
    }

    List& operator=(const List& other) {
        if (&other == this) return *this;
        size_t cnt = 0;
        try {
            for (const_iterator it = other.begin(); it != other.end(); it++) {
                push_back(*it);
                cnt++;
            }
        } catch (...) {
            for (size_t i = 0; i < cnt; i++) pop_back();
            throw;
        }
        while (sz > cnt) pop_front();

        if (std::allocator_traits<
                Allocator>::propagate_on_container_copy_assignment::value) {
            alloc = other.alloc;
            node_alloc = other.node_alloc;
        }
        return *this;
    }

    Allocator& get_allocator() { return alloc; }
    const Allocator& get_allocator() const { return alloc; }

    using iterator = BaseIterator<T>;
    using const_iterator = BaseIterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() { return iterator(end_node.next); }
    const_iterator begin() const { return cbegin(); }
    iterator end() { return iterator(&end_node); }
    const_iterator end() const { return cend(); }
    const_iterator cbegin() const { return const_iterator(end_node.next); }
    const_iterator cend() const { return const_iterator(end_node.next->prev); }
    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const { return crbegin(); }
    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const { return crend(); }
    const_reverse_iterator crbegin() const {
        return const_reverse_iterator(cend());
    }
    const_reverse_iterator crend() const {
        return const_reverse_iterator(cbegin());
    }

    size_t size() const { return sz; }

    iterator insert(const_iterator pos, const T& value) {
        Node* node = allocNode();
        try {
            std::allocator_traits<Allocator>::construct(alloc, &(node->value),
                                                        value);
        } catch (...) {
            deallocNode(node);
            throw;
        }
        node->prev = pos.node->prev;
        node->next = pos.node;
        node->next->prev = node;
        node->prev->next = node;
        sz++;
        return iterator(node);
    }

    void push_back(const T& value) { insert(const_iterator(&end_node), value); }
    void push_front(const T& value) {
        insert(const_iterator(end_node.next), value);
    }

    iterator erase(const_iterator pos) {
        Node* node = static_cast<Node*>(pos.node);
        node->~Node();
        iterator res(node->next);
        deallocNode(node);
        sz--;
        return res;
    }
    void pop_front() { erase(iterator(end_node.next)); }
    void pop_back() { erase(iterator(end_node.prev)); }

    ~List() { clear(); }
};

template <typename Key, typename Value, typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {
   public:
    using NodeType = std::pair<Key, Value>;

   private:
    Alloc alloc;
    size_t sz = 0;
    float load = 1.0;

    struct Node {
        NodeType data;
        size_t hash;
    };

    std::list<NodeType> data;
    std::vector<std::list<NodeType>::iterator> buckets;

    template <typename T>
    class BaseIterator {
        std::list<NodeType>::iterator item;
    };

   public:
    void rehash(size_t count) {}
};
